; ����ƥ��ģʽ https://autohotkey.com/docs/commands/SetTitleMatchMode.htm
SetTitleMatchMode,2

;------------------------------------------------------------------------------------------------------------
; ��������
;------------------------------------------------------------------------------------------------------------
; �����л�ΪӢ�����룬������Ҫ�������Կ�ݼ�
; english
CapsLock::
Send, {Esc}
Send, {Ctrl down}{Shift down}{5}{Shift up}{Ctrl up}
return

; english
;LShift::
;Send, {Ctrl down}{Shift down}{5}{Shift up}{Ctrl up}
;return

; chinese
;RShift::
;Send, {Ctrl down}{Shift down}{6}{Shift up}{Ctrl up}
;return

Esc::CapsLock
CapsLock::Esc


;------------------------------------------------------------------------------------------------------------
; ���ټ����
;------------------------------------------------------------------------------------------------------------
^F4::
  WinActivate, GNUEmacs
return

^F5::
; firefox
if WinExist("Mozilla Firefox")
  WinActivate, Mozilla Firefox
else
  Run, "C:\Program Files\Mozilla Firefox\firefox.exe"
; chrome
;WinActivate, ahk_class Chrome_WidgetWin_1
;return


