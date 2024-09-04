@ECHO OFF
setlocal EnableDelayedExpansion
set LF=^

SET RUN_Montreal=n
SET RUN_Toronto=n
SET RUN_Vancouver=n
SET STATE_COUNT=0

SET /p RUN_Montreal="Run Montreal [y/N] (default - N)?: "
SET /p RUN_Toronto="Run Toronto [y/N] (default - N)?: "
SET /p RUN_Vancouver="Run Vancouver [y/N] (default - N)?: "

SET Montreal_CONF=".\MS13-hADA\configuration.yaml"
SET Toronto_CONF=".\TS13-hADA\configuration.yaml"
SET Vancouver_CONF=".\VS13-hADA\configuration.yaml"


echo ################################
echo		PopGen Run
echo ################################

IF %RUN_Montreal% == y (.\Python27\python.exe Python27\EasyPopGen.py %Montreal_CONF%)
IF %RUN_Montreal% == y (
	SET STATE_COUNT=1
	IF %ERRORLEVEL% NEQ 0 (
	    SET RUN_Montreal = n
		SET LOG=%LOG%Montreal :: FAILED!!!;
	)
	IF %ERRORLEVEL% EQU 0 (
		SET LOG=%LOG%Montreal :: SUCCESS;
	)
)
IF NOT %RUN_Montreal% == y (
	SET LOG=%LOG%Montreal :: SKIPPED;
)

IF %RUN_Toronto% == y (.\Python27\python.exe Python27\EasyPopGen.py %Toronto_CONF%)
IF %RUN_Toronto% == y (
	SET STATE_COUNT=1
	IF %ERRORLEVEL% NEQ 0 (
	    SET RUN_Toronto = n
		SET LOG=%LOG%Toronto :: FAILED!!!;
	)
	IF %ERRORLEVEL% EQU 0 (
		SET LOG=%LOG%Toronto :: SUCCESS;
	)
)
IF NOT %RUN_Toronto% == y (
	SET LOG=%LOG%Toronto :: SKIPPED;
)

IF %RUN_Vancouver% == y (.\Python27\python.exe Python27\EasyPopGen.py %Vancouver_CONF%)
IF %RUN_Vancouver% == y (
	SET STATE_COUNT=1
	IF %ERRORLEVEL% NEQ 0 (
	    SET RUN_Vancouver = n
		SET LOG=%LOG%;Vancouver :: FAILED!!!;
	)
	IF %ERRORLEVEL% EQU 0 (
		SET LOG=%LOG%;Vancouver :: SUCCESS;
	)
)
IF NOT %RUN_Vancouver% == y (
	SET LOG=%LOG%Vancouver :: SKIPPED;
)
REM perpend summary in the log
SET LOG=SUMMARYE;%LOG%
FOR /F "delims=" %%a in ("%LOG:;=!LF!%") do (
  ECHO %%a
)

REM	################################
REM     If nothing is run then exit
REM ################################
IF %STATE_COUNT% == 0 (
	pause
	Exit
)


pause