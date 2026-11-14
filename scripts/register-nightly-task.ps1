# Registers a Windows Task Scheduler job that runs `tickwire nightly` once a day.
#
# Nothing is registered until you run this script yourself:
#   powershell -ExecutionPolicy Bypass -File scripts\register-nightly-task.ps1
# Remove it again with:
#   Unregister-ScheduledTask -TaskName "tickwire nightly" -Confirm:$false
#
# The task runs as the current user, only while logged on, with no elevated
# rights.  It runs the already-built executable, so rebuild with `dune build`
# after changing the code.

param(
    [string]$At = "02:00",          # local start time, HH:mm
    [int]$Minutes = 60,             # minutes of live data to capture
    [string]$TaskName = "tickwire nightly"
)

$ErrorActionPreference = "Stop"

$project = Split-Path -Parent $PSScriptRoot
$exe = Join-Path $project "_build\default\bin\main.exe"
if (-not (Test-Path $exe)) {
    throw "Build the project first (opam exec --switch=tickwire -- dune build). Missing: $exe"
}

$logDir = Join-Path $project "reports"
New-Item -ItemType Directory -Force $logDir | Out-Null
$log = Join-Path $logDir "nightly-last-run.log"

# cmd /c so stdout and stderr of the run land in a log file next to the reports.
# conhost --headless runs it with no visible window (a visible one invites being closed, which stops the run).
$arguments = "--headless cmd.exe /c `"`"$exe`" nightly --minutes $Minutes > `"$log`" 2>&1`""

$action = New-ScheduledTaskAction -Execute "conhost.exe" -Argument $arguments -WorkingDirectory $project
$trigger = New-ScheduledTaskTrigger -Daily -At $At
$settings = New-ScheduledTaskSettingsSet `
    -StartWhenAvailable `
    -ExecutionTimeLimit (New-TimeSpan -Minutes ($Minutes + 30)) `
    -MultipleInstances IgnoreNew
$principal = New-ScheduledTaskPrincipal -UserId $env:USERNAME -LogonType Interactive -RunLevel Limited

Register-ScheduledTask -TaskName $TaskName -Action $action -Trigger $trigger `
    -Settings $settings -Principal $principal `
    -Description "Capture $Minutes min of Kraken market data and write reports\YYYY-MM-DD\report.html (public data only, no trading)." `
    -Force | Out-Null

Write-Host "Registered '$TaskName': daily at $At, $Minutes minutes of capture."
Write-Host "Reports: $logDir\YYYY-MM-DD\report.html   Last run log: $log"
