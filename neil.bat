@echo off
set ARGS=%*
set SCRIPT=%~dp0neil
bb -f %SCRIPT% %ARGS%
