c-------------------------------------------------------------------------------------------------c
      program project_euler_76
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  It is possible to write five as a sum in exactly six different ways:                           c
c                                                                                                 c
c  4 + 1                                                                                          c
c  3 + 2                                                                                          c
c  3 + 1 + 1                                                                                      c
c  2 + 2 + 1                                                                                      c
c  2 + 1 + 1 + 1                                                                                  c
c  1 + 1 + 1 + 1 + 1                                                                              c
c                                                                                                 c
c  How many different ways can one hundred be written as a sum of at least two positive integers? c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 m(100), test, result, sub_result, num, dummy
      
c initialize
      num = 100
      result = 0

c start the huge loop 1-10
      m(1) = num-1
      do x1=m(1),0,-1
        sub_result = 0
        m(2) = num-x1
        do x2=min(m(2),x1),0,-1
          m(3) = m(2)-x2
          do x3=min(m(3),x2),0,-1
            m(4) = m(3)-x3
            do x4=min(m(4),x3),0,-1
              m(5) = m(4)-x4
              do x5=min(m(5),x4),0,-1
                m(6) = m(5)-x5
                do x6=min(m(6),x5),0,-1
                  m(7) = m(6)-x6
                  do x7=min(m(7),x6),0,-1
                    m(8) = m(7)-x7
                    do x8=min(m(8),x7),0,-1
                      m(9) = m(8)-x8
                      do x9=min(m(9),x8),0,-1
                        m(10) = m(9)-x9
                        do x10=min(m(10),x9),0,-1

c 11-20
      m(11) = m(10)-x10
      do x11=min(m(11),x10),0,-1
        m(12) = m(11)-x11
        do x12=min(m(12),x11),0,-1
          m(13) = m(12)-x12
          do x13=min(m(13),x12),0,-1
            m(14) = m(13)-x13
            do x14=min(m(14),x13),0,-1
              m(15) = m(14)-x14
              do x15=min(m(15),x14),0,-1
                m(16) = m(15)-x15
                do x16=min(m(16),x15),0,-1
                  m(17) = m(16)-x16
                  do x17=min(m(17),x16),0,-1
                    m(18) = m(17)-x17
                    do x18=min(m(18),x17),0,-1
                      m(19) = m(18)-x18
                      do x19=min(m(19),x18),0,-1
                        m(20) = m(19)-x19
                        do x20=min(m(20),x19),0,-1

c 21-30
      m(21) = m(20)-x20
      do x21=min(m(21),x20),0,-1
        m(22) = m(21)-x21
        do x22=min(m(22),x21),0,-1
          m(23) = m(22)-x22
          do x23=min(m(23),x22),0,-1
            m(24) = m(23)-x23
            do x24=min(m(24),x23),0,-1
              m(25) = m(24)-x24
              do x25=min(m(25),x24),0,-1
                m(26) = m(25)-x25
                do x26=min(m(26),x25),0,-1
                  m(27) = m(26)-x26
                  do x27=min(m(27),x26),0,-1
                    m(28) = m(27)-x27
                    do x28=min(m(28),x27),0,-1
                      m(29) = m(28)-x28
                      do x29=min(m(29),x28),0,-1
                        m(30) = m(29)-x29
                        do x30=min(m(30),x29),0,-1
                          
c 31-40
      m(31) = m(30)-x30
      do x31=min(m(31),x30),0,-1
        m(32) = m(31)-x31
        do x32=min(m(32),x31),0,-1
          m(33) = m(32)-x32
          do x33=min(m(33),x32),0,-1
            m(34) = m(33)-x33
            do x34=min(m(34),x33),0,-1
              m(35) = m(34)-x34
              do x35=min(m(35),x34),0,-1
                m(36) = m(35)-x35
                do x36=min(m(36),x35),0,-1
                  m(37) = m(36)-x36
                  do x37=min(m(37),x36),0,-1
                    m(38) = m(37)-x37
                    do x38=min(m(38),x37),0,-1
                      m(39) = m(38)-x38
                      do x39=min(m(39),x38),0,-1
                        m(40) = m(39)-x39
                        do x40=min(m(40),x39),0,-1

c 41-50
      m(41) = m(40)-x40
      do x41=min(m(41),x40),0,-1
        m(42) = m(41)-x41
        do x42=min(m(42),x41),0,-1
          m(43) = m(42)-x42
          do x43=min(m(43),x42),0,-1
            m(44) = m(43)-x43
            do x44=min(m(44),x43),0,-1
              m(45) = m(44)-x44
              do x45=min(m(45),x44),0,-1
                m(46) = m(45)-x45
                do x46=min(m(46),x45),0,-1
                  m(47) = m(46)-x46
                  do x47=min(m(47),x46),0,-1
                    m(48) = m(47)-x47
                    do x48=min(m(48),x47),0,-1
                      m(49) = m(48)-x48
                      do x49=min(m(49),x48),0,-1
                        m(50) = m(49)-x49
                        do x50=min(m(50),x49),0,-1

c 51-60
      m(51) = m(50)-x50
      do x51=min(m(51),x50),0,-1
        m(52) = m(51)-x51
        do x52=min(m(52),x51),0,-1
          m(53) = m(52)-x52
          do x53=min(m(53),x52),0,-1
            m(54) = m(53)-x53
            do x54=min(m(54),x53),0,-1
              m(55) = m(54)-x54
              do x55=min(m(55),x54),0,-1
                m(56) = m(55)-x55
                do x56=min(m(56),x55),0,-1
                  m(57) = m(56)-x56
                  do x57=min(m(57),x56),0,-1
                    m(58) = m(57)-x57
                    do x58=min(m(58),x57),0,-1
                      m(59) = m(58)-x58
                      do x59=min(m(59),x58),0,-1
                        m(60) = m(59)-x59
                        do x60=min(m(60),x59),0,-1

c 61-70
      m(61) = m(60)-x60
      do x61=min(m(61),x60),0,-1
        m(62) = m(61)-x61
        do x62=min(m(62),x61),0,-1
          m(63) = m(62)-x62
          do x63=min(m(63),x62),0,-1
            m(64) = m(63)-x63
            do x64=min(m(64),x63),0,-1
              m(65) = m(64)-x64
              do x65=min(m(65),x64),0,-1
                m(66) = m(65)-x65
                do x66=min(m(66),x65),0,-1
                  m(67) = m(66)-x66
                  do x67=min(m(67),x66),0,-1
                    m(68) = m(67)-x67
                    do x68=min(m(68),x67),0,-1
                      m(69) = m(68)-x68
                      do x69=min(m(69),x68),0,-1
                        m(70) = m(69)-x69
                        do x70=min(m(70),x69),0,-1

c 71-80
      m(71) = m(70)-x70
      do x71=min(m(71),x70),0,-1
        m(72) = m(71)-x71
        do x72=min(m(72),x71),0,-1
          m(73) = m(72)-x72
          do x73=min(m(73),x72),0,-1
            m(74) = m(73)-x73
            do x74=min(m(74),x73),0,-1
              m(75) = m(74)-x74
              do x75=min(m(75),x74),0,-1
                m(76) = m(75)-x75
                do x76=min(m(76),x75),0,-1
                  m(77) = m(76)-x76
                  do x77=min(m(77),x76),0,-1
                    m(78) = m(77)-x77
                    do x78=min(m(78),x77),0,-1
                      m(79) = m(78)-x78
                      do x79=min(m(79),x78),0,-1
                        m(80) = m(79)-x79
                        do x80=min(m(80),x79),0,-1

c 81-90
      m(81) = m(80)-x80
      do x81=min(m(81),x80),0,-1
        m(82) = m(81)-x81
        do x82=min(m(82),x81),0,-1
          m(83) = m(82)-x82
          do x83=min(m(83),x82),0,-1
            m(84) = m(83)-x83
            do x84=min(m(84),x83),0,-1
              m(85) = m(84)-x84
              do x85=min(m(85),x84),0,-1
                m(86) = m(85)-x85
                do x86=min(m(86),x85),0,-1
                  m(87) = m(86)-x86
                  do x87=min(m(87),x86),0,-1
                    m(88) = m(87)-x87
                    do x88=min(m(88),x87),0,-1
                      m(89) = m(88)-x88
                      do x89=min(m(89),x88),0,-1
                        m(90) = m(89)-x89
                        do x90=min(m(90),x89),0,-1

c 91-100
      m(91) = m(90)-x90
      do x91=min(m(91),x90),0,-1
        m(92) = m(91)-x91
        do x92=min(m(92),x91),0,-1
          m(93) = m(92)-x92
          do x93=min(m(93),x92),0,-1
            m(94) = m(93)-x93
            do x94=min(m(94),x93),0,-1
              m(95) = m(94)-x94
              do x95=min(m(95),x94),0,-1
                m(96) = m(95)-x95
                do x96=min(m(96),x95),0,-1
                  m(97) = m(96)-x96
                  do x97=min(m(97),x96),0,-1
                    m(98) = m(97)-x97
                    do x98=min(m(98),x97),0,-1
                      m(99) = m(98)-x98
                      do x99=min(m(99),x98),0,-1
                        m(100) = m(99)-x99
                        do x100=min(m(100),x99),0,-1
                                                    
                          test = x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18+x19+x20+
     .  x21+x22+x23+x24+x25+x26+x27+x28+x29+x30+x31+x32+x33+x34+x35+x36+x37+x38+x39+x40+
     .  x41+x42+x43+x44+x45+x46+x47+x48+x49+x50+x51+x52+x53+x54+x55+x56+x57+x58+x59+x60+
     .  x61+x62+x63+x64+x65+x66+x67+x68+x69+x70+x71+x72+x73+x74+x75+x76+x77+x78+x79+x80+
     .  x81+x82+x83+x84+x85+x86+x87+x88+x89+x90+x91+x92+x93+x94+x95+x96+x97+x98+x99+x100
                          if (test.eq.num) then
                            result = result + 1
                            sub_result = sub_result + 1
C                             write(*,fmt='(10(I2,1X))') x1,x2,x3,x4,x5,x6,x7,x8,x9,x10
C                             if ((result/100000)*100000.eq.result) then
C                               write(*,*) result
C                             endif
                          endif
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
                        enddo
                      enddo
                    enddo
                  enddo
                enddo
              enddo
            enddo
          enddo
        enddo
        write(*,*) x1,sub_result,result
      enddo
      
      write(*,*) 'number of ways to write ',num,' is ',result
      end