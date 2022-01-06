
      subroutine tinkerbox_getxyz(xyzfile_c, xyzfile_string_length)
      use inform
      use iounit
      use output
      implicit none

      integer*4, intent(in) :: xyzfile_string_length
      character, intent(in) :: xyzfile_c(xyzfile_string_length)
      character*240 :: xyzfile
      integer ixyz
      integer freeunit
      logical exist
      
c
c     Deal with fortran string, check xyzfile exist
c
      xyzfile = transfer(xyzfile_c(1:xyzfile_string_length), xyzfile)
      ! Henry 20220106: This is where keyfile is read in.
      ! basefile() -> getkey() -> keyline
      ! Parameter file is read in later from mechanic() -> field() -> getprm()
      call basefile (xyzfile)
      call suffix (xyzfile,'xyz','old')
      inquire (file=xyzfile,exist=exist)
      if (.not. exist)  call fatal
c
c     first open and then read the Cartesian coordinates file
c
      coordtype = 'CARTESIAN'
      ixyz = freeunit ()
      open (unit=ixyz,file=xyzfile,status='old')
      rewind (unit=ixyz)
      call readxyz (ixyz)
      close (unit=ixyz)
c
c     quit if the Cartesian coordinates file contains no atoms
c
      if (abort) then
         write (iout,30)
   30    format (/,' GETXYZ  --  Cartesian Coordinates File',
     &              ' does not Contain Any Atoms')
         call fatal
      end if

      return
      end

      