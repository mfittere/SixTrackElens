      subroutine abend
      implicit none
      save
*     just in case it is needed
      stop 99
      end
      subroutine datime(i1,i2)
      implicit none
      integer i1,i2
      save
      return
      end
      subroutine timest(r1)
      implicit none
      real r1
      save
      return
      end
      subroutine timex(r1)
      implicit none
      real r1
      save
      r1=0.
      return
      end
