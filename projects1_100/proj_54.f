c-------------------------------------------------------------------------------------------------c
      program project_euler_54
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  In the card game poker, a hand consists of five cards and are ranked, from lowest to highest,  c
c  in the following way:                                                                          c
c                                                                                                 c
c     High Card: Highest value card.                                                              c
c     One Pair: Two cards of the same value.                                                      c
c     Two Pairs: Two different pairs.                                                             c
c     Three of a Kind: Three cards of the same value.                                             c
c     Straight: All cards are consecutive values.                                                 c
c     Flush: All cards of the same suit.                                                          c
c     Full House: Three of a kind and a pair.                                                     c
c     Four of a Kind: Four cards of the same value.                                               c
c     Straight Flush: All cards are consecutive values of same suit.                              c
c     Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.                                     c
c                                                                                                 c
c  The cards are valued in the order:                                                             c
c  2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.                                            c
c                                                                                                 c
c  If two players have the same ranked hands then the rank made up of the highest value wins; for c
c  example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie,   c
c  for example, both players have a pair of queens, then highest cards in each hand are compared  c
c  (see example 4 below); if the highest cards tie then the next highest cards are compared, and  c
c  so on.                                                                                         c
c                                                                                                 c
c  Consider the following five hands dealt to two players:                                        c
c  Hand	  	 Player 1	 	        Player 2	 	     Winner                                           c
c    1	 	5H 5C 6S 7S KD   	 2C 3S 8S 8D TD      Player 2                                         c
c         Pair of Fives      Pair of Eights                                                       c
c    2	 	5D 8C 9S JS AC     2C 5C 7D 8S QH      Player 1                                         c
c         Highest card Ace   Highest card Queen                                                   c
c    3	 	2D 9C AS AH AC	 	 3D 6D 7D TD QD      Player 2                                         c
c         Three Aces         Flush with Diamonds                                                  c
c    4	 	4D 6S 9H QH QC	 	 3D 6D 7H QD QS      Player 1                                         c
c         Pair of Queens     Pair of Queens                                                       c
c         Highest card Nine  Highest card Seven                                                   c
c    5	 	2H 2D 4C 4D 4S     3C 3D 3S 9S 9D      Player 1                                         c
c         Full House         Full House                                                           c
c         With Three Fours   With Three Threes                                                    c
c                                                                                                 c
c  The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of     c
c  the file contains ten cards (separated by a single space): the first five are Player 1's cards c
c  and the last five are Player 2's cards. You can assume that all hands are valid (no invalid    c
c  characters or repeated cards), each player's hand is in no specific order, and in each hand    c
c  there is a clear winner.                                                                       c
c                                                                                                 c
c  How many hands does Player 1 win?                                                              c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      character cards(20), suit(2,5)
      integer*8 handn(2,5), which, index, num_wins, highcard(2,10), lowpair(2)
      integer*8 n_highcards(2), highcard_order(2,5), dummy
      logical poker_hand(2,10), used(2,5), check_straight(2,2:14), winner

c initialize parameters
      num_wins = 0
      
c open up the file
      open(unit=42,file='input_54.txt')
      
c loop over the 1000 hands
      do x1=1,1000
        read(42,fmt='(10(A1,A1,1X))') (cards(x2), x2=1,20)
c        write(*,fmt='(10(A1,A1,1X))') (cards(x2), x2=1,20)
c        read(*,*)

c initialize all the hands and cards
        do x2=1,2
          do x3=1,10
            highcard(x2,x3) = 0
            poker_hand(x2,x3) = .false.
          enddo
          do x3=1,5
            used(x2,x3) = .false.
            highcard_order(x2,x3) = 0
          enddo
          lowpair(2) = 0
          do x3=2,14
            check_straight(x2,x3) = .false.
          enddo
        enddo

c parse out the cards read into hands
c numbers
        do x2=1,10
          if (x2.le.5) then
            which = 1
            index = x2
          else
            which = 2
            index = x2-5
          endif
          
          if (cards(2*x2-1).eq.'2') handn(which,index) = 2
          if (cards(2*x2-1).eq.'3') handn(which,index) = 3
          if (cards(2*x2-1).eq.'4') handn(which,index) = 4
          if (cards(2*x2-1).eq.'5') handn(which,index) = 5
          if (cards(2*x2-1).eq.'6') handn(which,index) = 6
          if (cards(2*x2-1).eq.'7') handn(which,index) = 7
          if (cards(2*x2-1).eq.'8') handn(which,index) = 8
          if (cards(2*x2-1).eq.'9') handn(which,index) = 9
          if (cards(2*x2-1).eq.'T') handn(which,index) = 10
          if (cards(2*x2-1).eq.'J') handn(which,index) = 11
          if (cards(2*x2-1).eq.'Q') handn(which,index) = 12
          if (cards(2*x2-1).eq.'K') handn(which,index) = 13
          if (cards(2*x2-1).eq.'A') handn(which,index) = 14
        enddo
c suits
        do x2=1,10
          if (x2.le.5) then
            suit(1,x2) = cards(2*x2)
          else
            suit(2,x2-5) = cards(2*x2)
          endif
        enddo
        
cc write out the hands
c        do x2=1,2
c          write(*,*) (handn(x2,x3), x3=1,5)
c          write(*,*) (suit(x2,x3), x3=1,5)
c        enddo
c        read(*,*)

c go through both hands and find which poker hands are valid
        do x2=1,2

c four of a kind
          do x3=1,2
            do x4=x3+1,3
              do x5=x4+1,4
                do x6=x5+1,5
                  if ((handn(x2,x3).eq.handn(x2,x4)).and.(handn(x2,x3).eq.handn(x2,x5)).and.
     .  (handn(x2,x3).eq.handn(x2,x6))) then
                    poker_hand(x2,8) = .true.
                    highcard(x2,8) = handn(x2,x3)
                    used(x2,x3) = .true.
                    used(x2,x4) = .true.
                    used(x2,x5) = .true.
                    used(x2,x6) = .true.
                  endif
                enddo
              enddo
            enddo
          enddo

c three of a kind
          if (.not.poker_hand(x2,8)) then
            do x3=1,3
              do x4=x3+1,4
                do x5=x4+1,5
                  if ((handn(x2,x3).eq.handn(x2,x4)).and.(handn(x2,x3).eq.handn(x2,x5))) then
                    poker_hand(x2,4) = .true.
                    highcard(x2,4) = handn(x2,x3)
                    used(x2,x3) = .true.
                    used(x2,x4) = .true.
                    used(x2,x5) = .true.
                  endif
                enddo
              enddo
            enddo

c two pair            
            if (.not.poker_hand(x2,4)) then
              do x3=1,4
                do x4=x3+1,5
                  do x5=1,4
                    do x6=x5+1,5
                      if ((handn(x2,x3).eq.handn(x2,x4)).and.(handn(x2,x5).eq.handn(x2,x6)).and.
     .  (handn(x2,x3).ne.handn(x2,x5))) then
                        poker_hand(x2,3) = .true.
                        used(x2,x3) = .true.
                        used(x2,x4) = .true.
                        used(x2,x5) = .true.
                        used(x2,x6) = .true.
                        if (handn(x2,x3).gt.handn(x2,x5)) then
                          highcard(x2,3) = handn(x2,x3)
                          lowpair(x2) = handn(x2,x5)
                        else
                          highcard(x2,3) = handn(x2,x5)
                          lowpair(x2) = handn(x2,x3)
                        endif
                      endif
                    enddo
                  enddo
                enddo
              enddo
              
c one pair
              if (.not.poker_hand(x2,3)) then
                do x3=1,4
                  do x4=x3+1,5
                    if (handn(x2,x3).eq.handn(x2,x4)) then
                      poker_hand(x2,2) = .true.
                      highcard(x2,2) = handn(x2,x3)
                      used(x2,x3) = .true.
                      used(x2,x4) = .true.
                    endif
                  enddo
                enddo
              endif
            endif
          endif
 
c high card (automatically valid)
          poker_hand(x2,1) = .true.
          n_highcards(x2) = 0
          do x3=1,5
            if (.not.used(x2,x3)) then
              n_highcards(x2) = n_highcards(x2) + 1
              highcard_order(x2,n_highcards(x2)) = handn(x2,x3)
            endif
          enddo
c sort the leftover cards
          do x3=1,n_highcards(x2)-1
            do x4=x3+1,n_highcards(x2)
              if (highcard_order(x2,x3).lt.highcard_order(x2,x4)) then
                dummy = highcard_order(x2,x3)
                highcard_order(x2,x3) = highcard_order(x2,x4)
                highcard_order(x2,x4) = dummy
              endif
            enddo
          enddo
          highcard(x2,1) = highcard_order(x2,1)

c straight
          do x3=1,5
            check_straight(x2,handn(x2,x3)) = .true.
          enddo
          do x3=2,10
            if (check_straight(x2,x3).and.check_straight(x2,x3+1).and.check_straight(x2,x3+2)
     .          .and.check_straight(x2,x3+3).and.check_straight(x2,x3+4)) then
              poker_hand(x2,5) = .true.
              highcard(x2,6) = x3+4
            endif
          enddo
          
c flush
          if ((suit(x2,1).eq.suit(x2,2)).and.(suit(x2,1).eq.suit(x2,3)).and.(suit(x2,1).eq.suit(x2,3))
     .        .and.(suit(x2,1).eq.suit(x2,4)).and.(suit(x2,1).eq.suit(x2,5))) then
            poker_hand(x2,6) = .true.
c order the flush cards
            n_highcards(x2) = 5
            do x3=1,5
              if (.not.used(x2,x3)) then
                highcard_order(x2,n_highcards(x2)) = handn(x2,x3)
              endif
            enddo
c sort the cards (in case both players have flushes)
            do x3=1,n_highcards(x2)-1
              do x4=x3+1,n_highcards(x2)
                if (highcard_order(x2,x3).lt.highcard_order(x2,x4)) then
                  dummy = highcard_order(x2,x3)
                  highcard_order(x2,x3) = highcard_order(x2,x4)
                  highcard_order(x2,x4) = dummy
                endif
              enddo
            enddo
            highcard(x2,1) = highcard_order(x2,1)
          endif

c full house
          if (poker_hand(x2,2).and.poker_hand(x2,4)) then
            poker_hand(x2,7) = .true.
            highcard(x2,7) = highcard(x2,4)
          endif
            

c straight flush
          if (poker_hand(x2,5).and.poker_hand(x2,6)) then
            poker_hand(x2,9) = .true.
            highcard(x2,9) = highcard(x2,5)
          endif

c royal flush
          if (poker_hand(x2,9).and.(highcard(x2,9).eq.14)) then
            poker_hand(x2,10) = .true.
            highcard(x2,10) = 14
          endif  
        enddo

c now that the hands are analyzed go through and see who won
        winner = .false.
        do x2=10,1,-1
c player 1 wins
          if (poker_hand(1,x2).and.(.not.poker_hand(2,x2)).and.(.not.winner)) then
            winner = .true.
            num_wins = num_wins + 1
          endif
c player 2 wins
          if ((.not.poker_hand(1,x2)).and.poker_hand(2,x2).and.(.not.winner)) then
            winner = .true.
          endif
c both players have the same hand
          if (poker_hand(1,x2).and.poker_hand(2,x2).and.(.not.winner)) then
c all the hands that can be determined by the value of the poker hand
            if ((x2.eq.9).or.(x2.eq.8).or.(x2.eq.7).or.(x2.eq.5).or.(x2.eq.4)) then
              if (highcard(1,x2).gt.highcard(2,x2)) then
                winner = .true.
                num_wins = num_wins + 1
              endif
              if (highcard(1,x2).lt.highcard(2,x2)) then
                winner = .true.
              endif
            endif
c all the hands that one needs to look at something else
c two pair
            if (x2.eq.3) then
              if ((highcard(1,x2).gt.highcard(2,x2)).or.
     .            ((highcard(1,x2).eq.highcard(2,x2)).and.(lowpair(1).gt.lowpair(2))).or.
     .            ((highcard(1,x2).eq.highcard(2,x2)).and.(lowpair(1).eq.lowpair(2)).and.
     .            (highcard_order(1,1).gt.highcard_order(2,1)))) then
                winner = .true.
                num_wins = num_wins + 1
              else
                winner = .true.
              endif
            endif
c flush or high card
            if ((x2.eq.6).or.(x2.eq.1)) then
              if ((highcard_order(1,1).gt.highcard_order(2,1)).or.
     .           ((highcard_order(1,1).eq.highcard_order(2,1)).and.(highcard_order(1,2).gt.highcard_order(2,2))).or.
     .           ((highcard_order(1,1).eq.highcard_order(2,1)).and.(highcard_order(1,2).eq.highcard_order(2,2)).and.
     .            (highcard_order(1,3).gt.highcard_order(2,3))).or.
     .           ((highcard_order(1,1).eq.highcard_order(2,1)).and.(highcard_order(1,2).eq.highcard_order(2,2)).and.
     .            (highcard_order(1,3).eq.highcard_order(2,3)).and.(highcard_order(1,4).gt.highcard_order(2,4))).or.
     .           ((highcard_order(1,1).eq.highcard_order(2,1)).and.(highcard_order(1,2).eq.highcard_order(2,2)).and.
     .            (highcard_order(1,3).eq.highcard_order(2,3)).and.(highcard_order(1,4).eq.highcard_order(2,4)).and.
     .            (highcard_order(1,5).gt.highcard_order(2,5)))) then
                winner = .true.
                num_wins = num_wins + 1
              else
                winner = .true.
              endif
            endif
c one pair
            if (x2.eq.2) then
              if ((highcard(1,x2).gt.highcard(2,x2)).or.
     .           ((highcard(1,x2).eq.highcard(2,x2)).and.(highcard_order(1,1).gt.highcard_order(2,1))).or.
     .           ((highcard(1,x2).eq.highcard(2,x2)).and.(highcard_order(1,1).eq.highcard_order(2,1)).and.
     .            (highcard_order(1,2).gt.highcard_order(2,2))).or.
     .           ((highcard(1,x2).eq.highcard(2,x2)).and.(highcard_order(1,1).eq.highcard_order(2,1)).and.
     .            (highcard_order(1,2).eq.highcard_order(2,2)).and.(highcard_order(1,3).gt.highcard_order(2,3)))) then
                winner = .true.
                num_wins = num_wins + 1
              else
                winner = .true.
              endif
            endif
          endif
        enddo 
c        write(*,*) num_wins
c        read(*,*)
      enddo
      close(42)
      
      write(*,*) 'Number of wins for player 1 ',num_wins
      
      end