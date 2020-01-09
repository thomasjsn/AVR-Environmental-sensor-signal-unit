'--------------------------------------------------------------
'                   Thomas Jensen | stdout.no
'--------------------------------------------------------------
'  file: AVR_OESU_v.2.1 0x15
'  date: 08/03/2011
'  prot: 2.0            0x14
'  sn# : 84             0x54
'--------------------------------------------------------------
$regfile = "m8def.dat"
$crystal = 8000000
$baud = 38400
Config Portb = Output
Config Portd.5 = Output
Config Portd.6 = Output
Config Portd.7 = Output
Config Portc.2 = Input
Config Portc.3 = Input
Config Watchdog = 128

'serial
'PD0: Rx
'PD1: Tx

Dim Send As String * 11 , Stored_id As Eram Byte
Dim Serialcharwaiting As Byte , Serialchar As Byte
Dim Comminput As String * 9 , Out_value As Word
Dim Com_com As String * 1 , Com_nr As String * 1
Dim Led As Byte , Com_stat As String * 4 , Status As Byte
Dim Ws As String * 4 , W As Word , Com_value As Word
Dim Value As Word , Values As String * 4 , Id As Byte , Ids As String * 2

Config Timer1 = Pwm , Pwm = 8 , Prescale = 1 , Compare A Pwm = Clear Down
Config Adc = Single , Prescaler = Auto , Reference = Avcc
Start Adc

Const Min_id = 32
Const Max_id = 125
Const Pwm_max = 255

If Stored_id >= Min_id And Stored_id <= Max_id Then Id = Stored_id Else Id = Min_id

Ids = Hex(id)                                               'module id number
Const Status_verfirm = "15"                                 'status version firmware
Const Status_verprot = "14"                                 'status version protocol
Const Status_serial = "0054"                                'serial number
Const Status_di = "2"                                       'digital inputs
Const Status_do = "5"                                       'digital outputs
Const Status_ai = "2"                                       'analog inputs
Const Status_ao = "1"                                       'analog outputs

Start Watchdog
Set Status.0
If Id = Min_id Then Set Status.1

Main:
Serialcharwaiting = Ischarwaiting()

If Serialcharwaiting = 1 Then
   Serialchar = Inkey()
      If Serialchar = Id Or Serialchar = 126 Then
      Led = 203
      Goto Set_value
      End If
   End If

'led timer
If Led > 0 Then Decr Led                                    'activity LED
If Led = 200 Then Portd.5 = 1
If Led = 0 Then Portd.5 = 0

'green & red led
If Status > 0 Then Portd.6 = 1 Else Portd.6 = 0
Portd.7 = Not Portd.6

Reset Watchdog
Waitus 50
Goto Main
End

'serial receive
Set_value:
Input Comminput Noecho                                      'read serialport

Com_com = Mid(comminput , 2 , 1)                            'command check
Com_nr = Mid(comminput , 4 , 1)                             'output nr check
Com_stat = Mid(comminput , 6 , 4)                           'output full check
Com_value = Hexval(com_stat)

'output
If Com_com = "o" Then
Select Case Com_nr

'set digital output status
Case "0"
If Com_stat <> "" Then
Out_value = Com_value

If Out_value >= 16 Then                                     'digital output 5
   Portb.5 = 1
   Reset Out_value.4
   Else
   Portb.5 = 0
   End If
If Out_value >= 8 Then                                      'digital output 4
   Portb.4 = 1
   Reset Out_value.3
   Else
   Portb.4 = 0
   End If
If Out_value >= 4 Then                                      'digital output 3
   Portb.3 = 1
   Reset Out_value.2
   Else
   Portb.3 = 0
   End If
If Out_value >= 2 Then                                      'digital output 2
   Portb.2 = 1
   Reset Out_value.1
   Else
   Portb.2 = 0
   End If
If Out_value >= 1 Then                                      'digital output 1
   Portb.0 = 1
   Else
   Portb.0 = 0
   End If
End If

'get digital output status
Value = 0
If Portb.0 = 1 Then Set Value.0                             'digital output 1
If Portb.2 = 1 Then Set Value.1                             'digital output 2
If Portb.3 = 1 Then Set Value.2                             'digital output 3
If Portb.4 = 1 Then Set Value.3                             'digital output 4
If Portb.5 = 1 Then Set Value.4                             'digital output 5
   Values = Hex(value)
   If Len(values) < 2 Then Values = "000" + Values
   If Len(values) < 3 Then Values = "00" + Values
   If Len(values) < 4 Then Values = "0" + Values
Send = Ids + ",o,0," + Values
Print Send
Goto Main

Case "1"                                                    'analog out 1
If Com_stat <> "" Then
   If Com_value > Pwm_max Then Com_value = Pwm_max
   Pwm1a = Com_value
   End If
Ws = Hex(pwm1a)
If Len(ws) < 2 Then Ws = "000" + Ws
If Len(ws) < 3 Then Ws = "00" + Ws
If Len(ws) < 4 Then Ws = "0" + Ws
Send = Ids + ",o,1," + Ws
Print Send

End Select
Goto Main
End If

If Com_com = "i" Then
Select Case Com_nr

Case "0"                                                    'get digital input status
Value = 0
If Pinc.2 = 0 Then Set Value.0                              'digital input 1
If Pinc.3 = 0 Then Set Value.1                              'digital input 2
   Values = Hex(value)
   If Len(values) < 2 Then Values = "000" + Values
   If Len(values) < 3 Then Values = "00" + Values
   If Len(values) < 4 Then Values = "0" + Values
Send = Ids + ",i,0," + Values
Print Send
Goto Main

Case "1"                                                    'analog input 1
   W = Getadc(0)
   Ws = Hex(w)
   If Len(ws) < 2 Then Ws = "000" + Ws
   If Len(ws) < 3 Then Ws = "00" + Ws
   If Len(ws) < 4 Then Ws = "0" + Ws
   Send = Ids + ",i,1," + Ws
   Print Send

Case "2"                                                    'analog input 2
   W = Getadc(1)
   Ws = Hex(w)
   If Len(ws) < 2 Then Ws = "000" + Ws
   If Len(ws) < 3 Then Ws = "00" + Ws
   If Len(ws) < 4 Then Ws = "0" + Ws
   Send = Ids + ",i,2," + Ws
   Print Send

End Select
Goto Main
End If

'status
If Com_com = "s" Then                                       'read status
Select Case Com_nr

Case "0"
If Com_stat <> "" Then
Out_value = Com_value

If Out_value >= 4 Then
   Toggle Status.2
   Reset Out_value.2
   End If
If Out_value >= 2 Then
   Reset Status.1
   Reset Out_value.1
   End If
If Out_value >= 1 Then
   Reset Status.0
   End If
End If

Ws = Hex(status)
If Len(ws) < 2 Then Ws = "000" + Ws
If Len(ws) < 3 Then Ws = "00" + Ws
If Len(ws) < 4 Then Ws = "0" + Ws
Send = Ids + ",s,0," + Ws
Print Send

Case "1"
   Send = Ids + ",s,1," + Status_verfirm
   Print Send
Case "2"
   Send = Ids + ",s,2," + Status_verprot
   Print Send
Case "3"
   Send = Ids + ",s,3," + Status_serial
   Print Send
Case "4"
   Send = Ids + ",s,4," + Status_di
   Print Send
Case "5"
   Send = Ids + ",s,5," + Status_do
   Print Send
Case "6"
   Send = Ids + ",s,6," + Status_ai
   Print Send
Case "7"
   Send = Ids + ",s,7," + Status_ao
   Print Send

Case "E"
   Send = Ids + ",s,E,0001"
   Print Send
   Wait 1

Case "F"
   If Com_value >= Min_id And Com_value <= Max_id Then
      Stored_id = Com_value
      Id = Stored_id
      End If
   Send = Ids + ",s,F,00" + Hex(id)
   Print Send
   If Ids <> Hex(id) Then Wait 1

End Select
End If

Goto Main
End
