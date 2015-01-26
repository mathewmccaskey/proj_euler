c-------------------------------------------------------------------------------------------------c
      program project_euler_59
c-------------------------------------------------------------------------------------------------c
c                                                                                                 c
c  Each character on a computer is assigned a unique code and the preferred standard is ASCII     c
c  (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk  c
c  (*) = 42, and lowercase k = 107.                                                               c
c                                                                                                 c
c  A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each   c
c  byte with a given value, taken from a secret key. The advantage with the XOR function is that  c
c  using the same encryption key on the cipher text, restores the plain text; for example,        c
c  65 XOR 42 = 107, then 107 XOR 42 = 65.                                                         c
c                                                                                                 c
c  For unbreakable encryption, the key is the same length as the plain text message, and the key  c
c  is made up of random bytes. The user would keep the encrypted message and the encryption key   c
c  in different locations, and without both "halves", it is impossible to decrypt the message.    c
c                                                                                                 c
c  Unfortunately, this method is impractical for most users, so the modified method is to use a   c
c  password as a key. If the password is shorter than the message, which is likely, the key is    c
c  repeated cyclically throughout the message. The balance for this method is using a             c
c  sufficiently long password key for security, but short enough to be memorable.                 c
c                                                                                                 c
c  Your task has been made easy, as the encryption key consists of three lower case characters.   c
c  Using input_59.txt (right click and 'Save Link/Target As...'), a file containing the encrypted c
c  ASCII codes, and the knowledge that the plain text must contain common English words, decrypt  c
c  the message and find the sum of the ASCII values in the original text.                         c
c                                                                                                 c
c-------------------------------------------------------------------------------------------------c
      implicit none
      include 'euler.inc'

c parameters used in this program only
      integer*8 code(1201), key(3), index, decrypt_num(1201), sum
      character decrypt_char(1201)
      logical found_word
      
c read in the encrypted code
      open(unit=42,file='input_59.txt')
      do x1=1,1201
        read(42,*) code(x1)
      enddo
      close(42)
      
c the key is three lower case letters, that means we loop through ASCII codes 97-122
      do x1=97,122
        do x2=97,122
          do x3=97,122
            key(1) = x1
            key(2) = x2
            key(3) = x3
            index = 1
            
c loop over all numbers in the encrypted code and do the XOR
            do x4=1,1201
              decrypt_num(x4) = xor(code(x4),key(index))
              decrypt_char(x4) = number_to_ASCII(decrypt_num(x4))
              index = index + 1
              if (index.eq.4) index = 1
            enddo

c try to find the word ' the ' (spaces included) to see if the message was decrypted
            found_word = .false.
            x4 = 1
            do while ((.not.found_word).and.(x4.lt.1197))
              if ((decrypt_char(x4).eq.' ').and.(decrypt_char(x4+1).eq.'t').and.(decrypt_char(x4+2).eq.'h')
     .              .and.(decrypt_char(x4+3).eq.'e').and.(decrypt_char(x4+4).eq.' ')) then
                found_word = .true.
              endif
              x4 = x4 + 1
            enddo

c if the word is found then write the ASCII codes for the key and the decrypted message
            if (found_word) then
              write(*,*) x1, x2, x3
              do x4=0,10
                write(*,fmt='(100(A1))') (decrypt_char(x5), x5=1+100*x4,100+100*x4)
              enddo
              write(*,fmt='(101(A1))') (decrypt_char(x5), x5=1101,1201)
              
c add up the ASCII values of the decrypted message
              sum = 0
              do x4=1,1201
                sum = sum + decrypt_num(x4)
              enddo
              write(*,*) 'The sum of the ASCII characters is ',sum
              
            endif  
          enddo
        enddo
      enddo
      
      end