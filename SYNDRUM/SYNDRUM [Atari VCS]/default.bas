 t = 0
 u = 0
 v = 0
 w = 0
 COLUBK = 0
 COLUPF = $80
 



title
 
 t = t + 1
 if t = 31 then t = 0
 rem AUDV0 = 2
 rem AUDC0 = 4
 rem AUDF0 = 30 - t


 playfield:
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ................................
 ..XXXX..XXXX..XXXX..XXXX..XXXX..
end

 player0:
 %00000000
 %00000000
 %00000000
 %11001110
 %01001000
 %01001000
 %01001000
 %11101110
end

 player1:
 %00001001
 %00001001
 %00001001
 %11101011
 %00100000
 %11100000
 %10000000
 %11100000
end
 player0x=130:player0y=10:player1x=138:player1y=10

 COLUBK = 0
 COLUPF = 14

 a = 5
 b = 8
 c = 10
 d = 5

 p = 14
 q = 14

jumphere


 COLUP0 = 216
 COLUP1 = 216

 AUDV0 = 0
 AUDV1 = 0

 if joy0up then goto __up0on
 goto __up0off
after1

 if joy0left then goto __left0on 
 goto __left0off
after2

 if joy0right then goto __right0on
 goto __right0off
after3

 if joy0down then goto __down0on
 goto __down0off
after4

 drawscreen
 goto jumphere

 

 rem kick

__up0on
 

 COLUPF = 30
 
 AUDV1 = a
 AUDC1 = 2
 AUDF1 = 10 - a
 
 if a > 0 then a = a - 1


 pfhline 2 9 5 on
 pfhline 2 8 5 on
 pfhline 2 7 5 on
 pfhline 2 6 5 on
 pfhline 2 5 5 on
 pfhline 2 4 5 on
 pfhline 2 3 5 on

 t = 0

 goto after1

__up0off
 a = 9
 if t < 7 then t = t + 1
 if t = 7 then pfhline 2 9 5 off
 if t = 6 then pfhline 2 8 5 off
 if t = 5 then pfhline 2 7 5 off
 if t = 4 then pfhline 2 6 5 off
 if t = 3 then pfhline 2 5 5 off
 if t = 2 then pfhline 2 4 5 off
 if t = 1 then pfhline 2 3 5 off
 goto after1

 rem snare

__left0on

 COLUPF = 78

 AUDV0 = b
 AUDC0 = 8
 AUDF0 = 2
 
 if b > 0 then b = b - 1


 pfhline 8 9 11 on
 pfhline 8 8 11 on
 pfhline 8 7 11 on
 pfhline 8 6 11 on
 pfhline 8 5 11 on
 pfhline 8 4 11 on
 pfhline 8 3 11 on

 u = 0

 goto after2

__left0off
 b = 5
 if u < 7 then u = u + 1
 if u = 7 then pfhline 8 9 11 off
 if u = 6 then pfhline 8 8 11 off
 if u = 5 then pfhline 8 7 11 off
 if u = 4 then pfhline 8 6 11 off
 if u = 3 then pfhline 8 5 11 off
 if u = 2 then pfhline 8 4 11 off
 if u = 1 then pfhline 8 3 11 off
 goto after2

__right0on

 COLUPF = 134
 
 AUDV1 = c
 AUDC1 = 8
 AUDF1 = 2
 
 if c > 0 then c = c - 1


 pfhline 14 9 17 on
 pfhline 14 8 17 on
 pfhline 14 7 17 on
 pfhline 14 6 17 on
 pfhline 14 5 17 on
 pfhline 14 4 17 on
 pfhline 14 3 17 on

 v = 0

 goto after3

__right0off
 c = 10
 if v < 7 then v = v + 1
 if v = 7 then pfhline 14 9 17 off
 if v = 6 then pfhline 14 8 17 off
 if v = 5 then pfhline 14 7 17 off
 if v = 4 then pfhline 14 6 17 off
 if v = 3 then pfhline 14 5 17 off
 if v = 2 then pfhline 14 4 17 off
 if v = 1 then pfhline 14 3 17 off
 goto after3


__down0on
  COLUPF = 218
 AUDV1 = d
 AUDC1 = 8
 AUDF1 = 15 - d
 
 if d > 0 then d = d - 1


 pfhline 20 9 23 on
 pfhline 20 8 23 on
 pfhline 20 7 23 on
 pfhline 20 6 23 on
 pfhline 20 5 23 on
 pfhline 20 4 23 on
 pfhline 20 3 23 on

 w = 0

 goto after4

__down0off
 d = 5
 if w < 7 then w = w + 1
 if w = 7 then pfhline 20 9 23 off
 if w  = 6 then pfhline 20 8 23 off
 if w = 5 then pfhline 20 7 23 off
 if w = 4 then pfhline 20 6 23 off
 if w = 3 then pfhline 20 5 23 off
 if w = 2 then pfhline 20 4 23 off
 if w = 1 then pfhline 20 3 23 off
 goto after4