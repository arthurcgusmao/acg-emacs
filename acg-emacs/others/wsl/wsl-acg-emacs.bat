@echo off

for %%a in (%*) do (
    set @filePath=%%~dpa
    set @file=%%~nxa
)

cd %@filePath%
bash -c "/home/arthurcgusmao/.local/bin/acg-emacs %@file%"
