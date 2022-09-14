unit JobSystem;

{$mode delphi}

interface

uses
  StateMachine, Generics.Collections, syncobjs, Classes, SysUtils;

type

  { TSpinLock }

  TSpinLock = class(TSynchroObject)
  private
    dest: cardinal;
  public
    constructor Create;

    procedure Acquire; override;
    procedure Release; override;
  end;

  { TJob }

  TJob<D> = class(TStateMachine)
  protected
    Awake: boolean; // used only by the manager

    procedure SetJobData(const Data: D); virtual; abstract;
  end;

  TAbstractJobManager<D> = class abstract
  protected
    function GetScheduledJob(): TJob<D>; virtual abstract;
    procedure PutScheduledJob(const Job: TJob<D>); virtual; abstract;
  end;

  { TJobWorker }

  TJobWorker<T> = class(TThread)
  private
  type
    ManagerType = TAbstractJobManager<T>;
  protected
    Manager: ManagerType;
  public
    constructor Create(const AManager: ManagerType);

    procedure Execute; override;
  end;

  { TJobManager }

  TJobManager<D> = class(TAbstractJobManager<D>)
  protected
  type
    DataType = D;
    JobType = TJob<D>;
    JobTypeClass = class of JobType;
    WorkerType = TJobWorker<D>;
  protected
    function CreateNewWorker(): WorkerType;
    procedure InitWorkerPool(const NumberOfThreads: shortint);

    function CreateNewJob(): JobType;
    procedure InitJobPool(const NumberOfJobs: shortint);

    procedure ResumeRandomWorker;
  protected
    JobClass: JobTypeClass;

    JobPool: TList<JobType>;
    JobPoolLock: TSpinLock;//TCriticalSection;
    WorkerPool: TList<WorkerType>;
    ActiveJobs: TQueue<JobType>;
    ActiveJobsLock: TSpinLock;//TCriticalSection;

    function GetScheduledJob: JobType; override;
    procedure PutScheduledJob(const Job: JobType); override;
  public
    constructor Create(const AJobClass: JobTypeClass;
      const ThreadCount: shortint = 16; const InitialJobPoolSize: shortint = 120);
      virtual;
    destructor Destroy; override;

    procedure AddJob(const NewData: DataType);
    function JobCount: integer;
  end;

implementation

{ TJobManager }

function TJobManager<D>.CreateNewWorker: WorkerType;
begin
  Result := WorkerType.Create(Self);

  // register it
  WorkerPool.Add(Result);
end;

procedure TJobManager<D>.InitWorkerPool(const NumberOfThreads: shortint);
var
  i: shortint;
begin
  for i := 1 to NumberOfThreads do
    CreateNewWorker();
end;

function TJobManager<D>.CreateNewJob: JobType;
begin
  Result := JobClass.Create();
  Result.Awake := False;

  // register it
  JobPool.Add(Result);
end;

procedure TJobManager<D>.InitJobPool(const NumberOfJobs: shortint);
var
  i: shortint;
begin
  for i := 1 to NumberOfJobs do
    CreateNewJob();
end;

procedure TJobManager<D>.ResumeRandomWorker;
var
  worker: WorkerType;
begin
  TThread.Yield;// Sleep(1);

  if ActiveJobs.Count = 0 then Exit;

  for worker in WorkerPool do
    if worker.Suspended then
    begin
      worker.Suspended := False;

      break;
    end;
end;

function TJobManager<D>.GetScheduledJob: JobType;
begin
  Result := nil;
  if ActiveJobs.Count = 0 then Exit;

  ActiveJobsLock.Acquire;
  try
    if ActiveJobs.Count <> 0 then
      Result := ActiveJobs.Dequeue;
  finally
    ActiveJobsLock.Release;
  end;
end;

procedure TJobManager<D>.PutScheduledJob(const Job: JobType);
begin
  if Job.IsFinished then
  begin
    // this thing is no longer needed to be processed, so just deactivate it
    Job.Awake := False;
  end
  else
  begin
    // return the thing back to the cycle (at the end)
    ActiveJobsLock.Acquire;
    try
      ActiveJobs.Enqueue(Job);
    finally
      ActiveJobsLock.Release;
    end;
  end;
end;

constructor TJobManager<D>.Create(const AJobClass: JobTypeClass;
  const ThreadCount: shortint; const InitialJobPoolSize: shortint);
begin
  JobClass := AJobClass;
  JobPool := TList<JobType>.Create;
  WorkerPool := TList<WorkerType>.Create;
  ActiveJobs := TQueue<JobType>.Create;

  JobPoolLock := TSpinLock.Create;
  ActiveJobsLock := TSpinLock.Create;

  InitWorkerPool(ThreadCount);
  InitJobPool(InitialJobPoolSize);
end;

destructor TJobManager<D>.Destroy;
var
  worker: WorkerType;
  job: JobType;
begin
  // stop all workers
  for worker in WorkerPool do
  begin
    worker.Terminate();
    if worker.Suspended then
      worker.Suspended := False;
    Sleep(1);

    worker.WaitFor;
    worker.Free;
  end;

  // clear ALL jobs
  for job in JobPool do
  begin
    if job.Awake then
      job.Cancell;

    job.Free;
  end;

  FreeAndNil(WorkerPool);
  FreeAndNil(ActiveJobs);
  FreeAndNil(JobPool);

  FreeAndNil(ActiveJobsLock);
  FreeAndNil(JobPoolLock);

  inherited Destroy;
end;

procedure TJobManager<D>.AddJob(const NewData: DataType);
var
  job, selectedJob: JobType;
begin
  selectedJob := nil;

  // find a Job item in the pool, assign data to it, and then awake it
  for job in JobPool do
    if not job.Awake then
    begin
      JobPoolLock.Acquire;
      try
        if not job.Awake then
        begin
          job.Reset;
          job.SetJobData(NewData);
          job.Awake := True;

          selectedJob := job;

          Break;
        end;
      finally
        JobPoolLock.Release;
      end;
    end;

  // instantiate a fresh new JOB object
  if not Assigned(selectedJob) then
  begin
    JobPoolLock.Acquire;
    try
      selectedJob := CreateNewJob();

      selectedJob.Reset;
      selectedJob.SetJobData(NewData);
      selectedJob.Awake := True;
    finally
      JobPoolLock.Release;
    end;
  end;

  // "start" working on the thing
  PutScheduledJob(selectedJob);
  ResumeRandomWorker;
end;

function TJobManager<D>.JobCount: integer;
begin
  Result := ActiveJobs.Count;
end;

{ TJobWorker }

constructor TJobWorker<T>.Create(const AManager: ManagerType);
begin
  inherited Create(False);
  Priority := tpLower;

  Manager := AManager;
end;

procedure TJobWorker<T>.Execute;
var
  job: TJob<T>;
begin
  while not Terminated do
  begin
    job := Manager.GetScheduledJob();

    if job <> nil then
    begin
      job.DoStep;

      Manager.PutScheduledJob(job);
    end
    else if not Terminated then
      Self.Suspended := True;//Sleep(5);
  end;
end;

{ TSpinLock }

constructor TSpinLock.Create;
begin
  dest := 0;
end;

procedure TSpinLock.Acquire;
begin
  while True do
  begin
    if InterlockedCompareExchange(dest, 100, 0) = 0 then
      Break
    else
      TThread.Yield;
  end;
end;

procedure TSpinLock.Release;
begin
  dest := 0;
end;

end.
