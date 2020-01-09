'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_OESU_v.1.1
'  date: 14/02/2010
'  prot: 1.3
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

'serial
'PD0: Rx
'PD1: Tx

Dim W1 As Word , W2 As Word
Dim Lifesignal As Word , Life As Integer , Send As String * 20
Dim Inn(4) As Byte , A As Byte , A2 As String * 1
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 15 , Input_nr As String * 3
Dim Input_com As String * 1 , Input_ut As String * 2
Dim Input_stat As String * 1 , Led As Byte , Ut(7) As Byte
Dim Ut_t(6) As Word , B As Byte
Dim W1s As String * 3 , W2s As String * 3

Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Id = "003"
Life = 1000

Waitms 5000

Top:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      Goto Myroutine
   End If

Goto Main

Myroutine:
Select Case Serialchar
Case 42                                                     '*
Goto Set_value
End Select

Main:
'input send off signal
For A = 3 To 4
   If Inn(a) = 1 Then
      Led = 103
      A2 = Str(a)
      If Len(a2) < 2 Then A2 = "0" + A2
      Send = Id + ":i:" + A2 + ":000"
      Print Send
      End If
Next A

'input send on signal
If Inn(1) = 1 Then                                          'input 1
   W1 = Getadc(0)
   If W1 > 999 Then W1 = 999
   Led = 103
   W1s = Str(w1)
   If Len(w1s) < 2 Then W1s = "0" + W1s
   If Len(w1s) < 3 Then W1s = "0" + W1s
   Send = Id + ":i:01:" + W1s
   Print Send
   Inn(1) = 0
   End If

If Inn(2) = 1 Then                                          'input 2
   W2 = Getadc(1)
   If W2 > 999 Then W2 = 999
   Led = 103
   W2s = Str(w2)
   If Len(w2s) < 2 Then W2s = "0" + W2s
   If Len(w2s) < 3 Then W2s = "0" + W2s
   Send = Id + ":i:02:" + W2s
   Print Send
   Inn(2) = 0
End If

If Pinc.2 = 0 Then                                          'input 3
   If Inn(3) = 0 Then
   Led = 103
   Send = Id + ":i:03:001"
   Print Send

   End If
   Inn(3) = 250
End If

If Pinc.3 = 0 Then                                          'input 4
   If Inn(4) = 0 Then
   Led = 103
   Send = Id + ":i:04:001"
   Print Send
   End If
   Inn(4) = 250
End If

'set input counters
For A = 3 To 4
   If Inn(a) > 0 Then Decr Inn(a)
Next A

'set output counters
For B = 1 To 6
   If Ut_t(b) > 0 Then Decr Ut_t(b)
Next B

'handle outputs
For B = 1 To 5
If Ut(b) = 1 And Ut_t(b) = 0 Then Ut_t(b) = 667
Next B
If Ut(6) = 2 And Ut_t(6) = 0 Then Ut_t(6) = 5001
If Ut(6) = 1 Then
   Ut_t(6) = 1201
   Ut(6) = 0
   Send = Id + ":o:06:000"
   Print Send
   End If

If Ut_t(1) = 334 And Ut(7) = 0 Then Portb.0 = 1
If Ut_t(1) = 1 Then Portb.0 = 0

If Ut_t(2) = 334 And Ut(7) = 0 Then Portb.1 = 1
If Ut_t(2) = 1 Then Portb.1 = 0

If Ut_t(3) = 334 And Ut(7) = 0 Then Portb.2 = 1
If Ut_t(3) = 1 Then Portb.2 = 0

If Ut_t(4) = 334 And Ut(7) = 0 Then Portb.3 = 1
If Ut_t(4) = 1 Then Portb.3 = 0

If Ut_t(5) = 334 And Ut(7) = 0 Then Portb.4 = 1
If Ut_t(5) = 1 Then Portb.4 = 0

If Ut_t(6) = 1101 And Ut(7) = 0 Then Portb.5 = 1
If Ut_t(6) = 701 Then Portb.5 = 0
If Ut_t(6) = 301 And Ut(7) = 0 Then Portb.5 = 1
If Ut_t(6) = 201 Then Portb.5 = 0
If Ut_t(6) = 101 And Ut(7) = 0 Then Portb.5 = 1
If Ut_t(6) = 1 Then Portb.5 = 0

'led timer
If Led > 0 Then Decr Led
If Led = 100 Then Portd.5 = 1
If Led = 0 Then Portd.5 = 0

'lifestring
If Life > 0 Then Decr Life
If Life = 0 Then
   Led = 103
   Send = Id + ":s:01:001"
   Print Send
   Life = 20000
   End If

'lifesignal
If Lifesignal > 0 Then Decr Lifesignal
If Lifesignal = 500 Then
   If Ut(7) = 1 Then Portd.6 = 1
   If Ut(7) = 0 Then Portd.7 = 1
   End If
If Lifesignal = 0 Then
   Portd.6 = 0
   Portd.7 = 0
   Lifesignal = 2100
   End If

Waitms 1
Goto Top
End

Set_value:
Input Comminput Noecho                                      'read serialport

Input_nr = Left(comminput , 3)                              'id check
Input_com = Mid(comminput , 5 , 1)                          'command check
Input_ut = Mid(comminput , 7 , 2)                           'output nr check
Input_stat = Mid(comminput , 10 , 1)                        'output stat check

'output
If Input_nr = Id Then

If Input_com = "o" Then
Led = 103
Select Case Input_ut

Case "01"                                                   'output 1
If Input_stat = "1" Then Ut(1) = 1
If Input_stat = "0" Then Ut(1) = 0
Send = Id + ":o:01:00" + Str(ut(1))
Print Send

Case "02"                                                   'output 2
If Input_stat = "1" Then Ut(2) = 1
If Input_stat = "0" Then Ut(2) = 0
Send = Id + ":o:02:00" + Str(ut(2))
Print Send

Case "03"                                                   'output 3
If Input_stat = "1" Then Ut(3) = 1
If Input_stat = "0" Then Ut(3) = 0
Send = Id + ":o:03:00" + Str(ut(3))
Print Send

Case "04"                                                   'output 4
If Input_stat = "1" Then Ut(4) = 1
If Input_stat = "0" Then Ut(4) = 0
Send = Id + ":o:04:00" + Str(ut(4))
Print Send

Case "05"                                                   'output 5
If Input_stat = "1" Then Ut(5) = 1
If Input_stat = "0" Then Ut(5) = 0
Send = Id + ":o:05:00" + Str(ut(5))
Print Send

Case "06"
If Input_stat = "2" Then Ut(6) = 2                          'output 6
If Input_stat = "1" Then Ut(6) = 1
If Input_stat = "0" Then Ut(6) = 0
Send = Id + ":o:06:00" + Str(ut(6))
Print Send

Case "07"                                                   'output 7
If Input_stat = "1" Then Ut(7) = 1
If Input_stat = "0" Then Ut(7) = 0
Send = Id + ":o:07:00" + Str(ut(7))
Print Send

End Select
End If

If Input_com = "i" Then
Select Case Input_ut

Case "01"
If Inn(1) = 0 Then Inn(1) = 1                               'status input 1
Case "02"
If Inn(2) = 0 Then Inn(2) = 1                               'status input 2
Case "03"
If Inn(3) = 0 Then Inn(3) = 2                               'status input 3
Case "04"
If Inn(4) = 0 Then Inn(4) = 2                               'status input 4

End Select
End If

End If
Goto Main
End
