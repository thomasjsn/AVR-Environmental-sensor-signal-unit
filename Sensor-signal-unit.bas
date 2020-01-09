'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_OESU_v.1.2
'  date: 31/07/2010
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 9600
Config Portb = Output
Config Portd.5 = Output
Config Portd.6 = Output
Config Portd.7 = Output
Config Portc.2 = Input
Config Portc.3 = Input
Config Watchdog = 1024

'serial
'PD0: Rx
'PD1: Tx

Dim Lifesignal As Word , Send As String * 20
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 15 , Din(2) As Byte
Dim Input_com As String * 1 , Input_ut As String * 2
Dim Input_stat As String * 1 , Led As Byte
Dim W1s As String * 3 , W2s As String * 3 , W1 As Word , W2 As Word
Dim All As Bit , A As Byte , Value As Byte

Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Id = "003"

Portd.6 = 1
Waitms 5000
Portd.6 = 0
Start Watchdog

Main:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      Select Case Serialchar
      Case 42                                               '*
      Goto Set_value
      End Select
   End If

'led timer
If Led > 0 Then Decr Led
If Led = 100 Then Portd.5 = 1
If Led = 0 Then Portd.5 = 0

'lifesignal
If Lifesignal > 0 Then Decr Lifesignal
If Lifesignal = 500 Then Portd.7 = 1
If Lifesignal = 0 Then
   Portd.7 = 0
   Lifesignal = 2100
   End If

Reset Watchdog
Goto Main
End

Set_value:
Input Comminput Noecho                                      'read serialport

Input_com = Mid(comminput , 5 , 1)                          'command check
Input_ut = Mid(comminput , 7 , 2)                           'output nr check
Input_stat = Mid(comminput , 10 , 1)                        'output stat check

If Input_com = "s" And Input_ut = "01" Then
All = 1


   For A = 1 To 6
   Input_ut = "0" + Str(a)
   Goto Outputs
   Next A
   For A = 1 To 4
   Input_ut = "0" + Str(a)
   Goto Inputs
   Next A
   Send = Id + ":s:01:" + Str(portb)
   Print Send
   Send = Id + ":s:02:" + Str(portb)
   Print Send

All = 0
Goto Main
End If

'output
If Input_com = "o" Then
Outputs:
Led = 103

If Portb.0 = 1 Then Value = Value + 1


Select Case Input_ut

Case "01"                                                   'output 1
If Input_stat = "1" Then Portb.0 = 1
If Input_stat = "0" Then Portb.0 = 0
Send = Id + ":o:01:00" + Str(portb.0)
Print Send

Case "02"                                                   'output 2
If Input_stat = "1" Then Portb.1 = 1
If Input_stat = "0" Then Portb.1 = 0
Send = Id + ":o:02:00" + Str(portb.1)
Print Send

Case "03"                                                   'output 3
If Input_stat = "1" Then Portb.2 = 1
If Input_stat = "0" Then Portb.2 = 0
Send = Id + ":o:03:00" + Str(portb.2)
Print Send

Case "04"                                                   'output 4
If Input_stat = "1" Then Portb.3 = 1
If Input_stat = "0" Then Portb.3 = 0
Send = Id + ":o:04:00" + Str(portb.3)
Print Send

Case "05"                                                   'output 5
If Input_stat = "1" Then Portb.4 = 1
If Input_stat = "0" Then Portb.4 = 0
Send = Id + ":o:05:00" + Str(portb.4)
Print Send

Case "06"
If Input_stat = "1" Then Portb.5 = 1                           'output 6
If Input_stat = "0" Then Portb.5 = 0
Send = Id + ":o:06:00" + Str(portb.5)
Print Send

If All = 1 Then Return
End Select
End If

If Input_com = "i" Then
Inputs:
Select Case Input_ut

Case "01"
   W1 = Getadc(0)
   If W1 > 999 Then W1 = 999
   Led = 103
   W1s = Str(w1)
   If Len(w1s) < 2 Then W1s = "0" + W1s
   If Len(w1s) < 3 Then W1s = "0" + W1s
   Send = Id + ":i:01:" + W1s
   Print Send
                                                     'status input 1
Case "02"
   W2 = Getadc(1)
   If W2 > 999 Then W2 = 999
   Led = 103
   W2s = Str(w2)
   If Len(w2s) < 2 Then W2s = "0" + W2s
   If Len(w2s) < 3 Then W2s = "0" + W2s
   Send = Id + ":i:02:" + W2s
   Print Send
                                                     'status input 2
Case "03"
   Led = 103
   Din(1) = Not Pinc.2
   Send = Id + ":i:03:00" + Str(din(1))
   Print Send
                                                     'status input 3
Case "04"
   Led = 103
   Din(2) = Not Pinc.3
   Send = Id + ":i:04:00" + Str(din(2))
   Print Send                                               'status input 4

If All = 1 Then Return
End Select
End If

Goto Main
End
