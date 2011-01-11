@echo off
cls
cl /Zi /nologo /D_CRT_SECURE_NO_WARNINGS /W3 zept_test.c zept_test2.c /link /out:zept_test.exe && zept_test.exe %*
