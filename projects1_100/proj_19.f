c-------------------------------------------------------------------------------------------------c
      program project_euler_19
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  You are given the following information, but you may prefer to do some research for yourself.  c
c                                                                                                 c
c    1 Jan 1900 was a Monday.                                                                     c
c    Thirty days has September,                                                                   c
c    April, June and November.                                                                    c
c    All the rest have thirty-one,                                                                c
c    Saving February alone,                                                                       c
c    Which has twenty-eight, rain or shine.                                                       c
c    And on leap years, twenty-nine.                                                              c
c    A leap year occurs on any year evenly divisible by 4, but not on a century unless it is      c
c    divisible by 400.                                                                            c
c                                                                                                 c
c  How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to    c
c  31 Dec 2000)?                                                                                  c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'
      
c parameters used in this subroutine only
      integer*8 year, month, day_of_week, num_sundays
      
c initialize the date
      year = 1901
      month = 1
      day_of_week = mod(1+365,7)
      num_sundays = 0
      
      do while ((year.le.2000).and.(month.le.12))

c add the length of each month to the date (including stupid leap years)
        if (month.eq.1) day_of_week = day_of_week + 31
        if (month.eq.2) then
          if (year.eq.2000) then
            day_of_week = day_of_week + 29
          else if (is_multiple(year,4)) then
            day_of_week = day_of_week + 29
          else
            day_of_week = day_of_week + 28
          endif
        endif
        if (month.eq.3) day_of_week = day_of_week + 31
        if (month.eq.4) day_of_week = day_of_week + 30
        if (month.eq.5) day_of_week = day_of_week + 31
        if (month.eq.6) day_of_week = day_of_week + 30
        if (month.eq.7) day_of_week = day_of_week + 31
        if (month.eq.8) day_of_week = day_of_week + 31
        if (month.eq.9) day_of_week = day_of_week + 30
        if (month.eq.10) day_of_week = day_of_week + 31
        if (month.eq.11) day_of_week = day_of_week + 30
        if (month.eq.12) day_of_week = day_of_week + 31

c get the day of the week
        day_of_week = mod(day_of_week,7)
        if (day_of_week.eq.0) num_sundays = num_sundays + 1
        
c increment the month
        month = month + 1
        if (month.eq.13) then
          month = 1
          year = year + 1
        endif
        
      enddo
      
      write(*,*) 'The number of Sundays on the 1st of the month is ',num_sundays
      
      end