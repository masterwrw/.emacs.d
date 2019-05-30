; 标题匹配模式 https://autohotkey.com/docs/commands/SetTitleMatchMode.htm
SetTitleMatchMode,2

;------------------------------------------------------------------------------------------------------------
; 交换按键
;------------------------------------------------------------------------------------------------------------
Home::CapsLock
CapsLock::Home

; LWin::End
; End::LWin

; AppsKey::LWin
; LWin::AppsKey

;------------------------------------------------------------------------------------------------------------
; 快速激活窗口
;------------------------------------------------------------------------------------------------------------
F1::
  WinActivate, Editor
return

F2::
if WinExist("Microsoft Visual Studio")
  WinActivate, Microsoft Visual Studio
else
  Run, "C:\Program Files (x86)\Microsoft Visual Studio 12.0\Common7\IDE\devenv.exe"
return

F3::
if WinExist("Mozilla Firefox")
  WinActivate, Mozilla Firefox
else
  Run, "C:\Program Files\Mozilla Firefox\firefox.exe"
return


;------------------------------------------------------------------------------------------------------------
; 快捷键
; $F5:: AltTabMenu()
; $F6:: AltTab()
; ! 	{Alt} 	{Alt down} 	 {Alt up}
; + 	{Shift} {Shift down} {Shift up}
; ^ 	{Ctrl} 	{Ctrl down}
; # 	{LWin}  {LWin down}  {LWin up}
;       {RWin}  {RWin down}  {RWin up}
;
;------------------------------------------------------------------------------------------------------------

; Alt+F4
#q::
Send {Alt down}{F4}
return

; 显示桌面
^+d::
Send, #d
return

; 文件管理器
^+e::
Send, #e
return

; 重命名
^+r::
Send, {AppsKey}m                ; 模拟右键菜单
return

; 在此处打开命令行
; 在资源管理器窗口有焦点才执行
; 没选中文件夹时，还需要按回车键才能打开命令行，如果选中了文件夹，则不需要再按回车，否则会再打开一个窗口
; 判断没有打开命令行时，按下回车键
^+c::
if WinActive("ahk_class CabinetWClass")
  Send, {Shift down}{AppsKey}w{Shift up}
  Sleep, 500
  IfWinNotActive, cmd.exe
    Send, {Enter}
return

;------------------------------------------------------------------------------------------------------------
; 切换窗口
; @see https://stackoverflow.com/questions/35971452/what-is-the-right-way-to-send-alt-tab-in-ahk
;------------------------------------------------------------------------------------------------------------
;F5::
;Run, "C:\Users\Default\AppData\Roaming\Microsoft\Internet Explorer\Quick Launch\Window Switcher.lnk"
;return


^+w:: AltTabMenu()
^+q:: AltTab()
^+x::
Send, {Alt down}{F4}{Alt up}
return

; AltTab-replacement for Windows 8:
AltTab(){
    list := ""
    WinGet, id, list
    Loop, %id%
    {
        this_ID := id%A_Index%
        IfWinActive, ahk_id %this_ID%
            continue    
        WinGetTitle, title, ahk_id %this_ID%
        If (title = "")
            continue
        If (!IsWindow(WinExist("ahk_id" . this_ID))) 
            continue
        WinActivate, ahk_id %this_ID%, ,2
            break
    }
}

; AltTabMenu-replacement for Windows 8:
AltTabMenu(){
    list := ""
    Menu, windows, Add
    Menu, windows, deleteAll
    WinGet, id, list
    Loop, %id%
    {
        this_ID := id%A_Index%
        WinGetTitle, title, ahk_id %this_ID%
        If (title = "")
            continue            
        If (!IsWindow(WinExist("ahk_id" . this_ID))) 
            continue
        Menu, windows, Add, %title%, ActivateTitle      
        WinGet, Path, ProcessPath, ahk_id %this_ID%
        Try 
            Menu, windows, Icon, %title%, %Path%,, 0
        Catch 
            Menu, windows, Icon, %title%, %A_WinDir%\System32\SHELL32.dll, 3, 0 
    }
    CoordMode, Mouse, Screen
    MouseMove, (0.4*A_ScreenWidth), (0.35*A_ScreenHeight)
    CoordMode, Menu, Screen
    Xm := (0.25*A_ScreenWidth)
    Ym := (0.25*A_ScreenHeight)
    Menu, windows, Show, %Xm%, %Ym%
}

ActivateTitle:
    SetTitleMatchMode 3
    WinActivate, %A_ThisMenuItem%
return

;-----------------------------------------------------------------
; Check whether the target window is activation target
;-----------------------------------------------------------------
IsWindow(hWnd){
    WinGet, dwStyle, Style, ahk_id %hWnd%
    if ((dwStyle&0x08000000) || !(dwStyle&0x10000000)) {
        return false
    }
    WinGet, dwExStyle, ExStyle, ahk_id %hWnd%
    if (dwExStyle & 0x00000080) {
        return false
    }
    WinGetClass, szClass, ahk_id %hWnd%
    if (szClass = "TApplication") {
        return false
    }
    return true
}

