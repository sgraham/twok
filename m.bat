@echo off
cls
cl /Zi /nologo /D_CRT_SECURE_NO_WARNINGS /W3 /WX twok_test.c twok_test2.c /link /out:twok_test.exe && twok_test.exe %*
ctags twok.h
