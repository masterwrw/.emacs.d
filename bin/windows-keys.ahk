; 标题匹配模式 https://autohotkey.com/docs/commands/SetTitleMatchMode.htm
SetTitleMatchMode,2

;------------------------------------------------------------------------------------------------------------
; 交换按键
;------------------------------------------------------------------------------------------------------------
; 按键切换为英文输入，首先需要设置语言快捷键
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
; 快速激活窗口
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


