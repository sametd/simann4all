module mersenne_twister
implicit none
integer, parameter :: dp = SELECTED_REAL_KIND(12, 60)
! Period parameters
integer, parameter :: n = 624, n1 = n+1, m = 397, mata = -1727483681
! constant vector a
integer, parameter :: umask = -2147483647 - 1
! most significant w-r bits
integer, parameter :: lmask =  2147483647
! least significant r bits
! Tempering parameters
integer, parameter :: tmaskb= -1658038656, tmaskc= -272236544
! the array for the state vector
integer, save      :: mt(0:n-1), mti = n1
! mti==N+1 means mt[N] is not initialized
private
public :: dp, sgrnd, grnd, init_genrand
contains
	subroutine sgrnd(seed)
		! This is the original version of the seeding routine.
		! It was replaced in the Japanese version in C on 26 January 2002
		! It is recommended that routine init_genrand is used instead.
		integer, intent(in)   :: seed
		!    setting initial seeds to mt[N] using the generator Line 25 of Table 1 in
		!    [KNUTH 1981, The Art of Computer Programming Vol. 2 (2nd Ed.), pp102]
		mt(0)= IAND(seed, -1)
		do  mti=1,n-1
		  mt(mti) = IAND(69069 * mt(mti-1), -1)
		end do
		return
	end subroutine sgrnd
!***********************************************************************
	subroutine init_genrand(seed)
		! This initialization is based upon the multiplier given on p.106 of the
		! 3rd edition of Knuth, The Art of Computer Programming Vol. 2.
		! This version assumes that integer overflow does NOT cause a crash.
		integer, intent(in)  :: seed
		integer  :: latest
		mt(0) = seed
		latest = seed
		do mti = 1, n-1
		  latest = IEOR( latest, ISHFT( latest, -30 ) )
		  latest = latest * 1812433253 + mti
		  mt(mti) = latest
		end do
		return
	end subroutine init_genrand
!***********************************************************************
	function grnd() result(fn_val)
		real (dp) :: fn_val
		integer, save :: mag01(0:1) = (/ 0, mata /)
		! mag01(x) = x * MATA for x=0,1
		integer       :: kk, y
		! These statement functions have been replaced with separate functions
		! tshftu(y) = ISHFT(y,-11)
		! tshfts(y) = ISHFT(y,7)
		! tshftt(y) = ISHFT(y,15)
		! tshftl(y) = ISHFT(y,-18)
		if(mti >= n) then
		! generate N words at one time
		  if(mti == n+1) then
		!if sgrnd() has not been called,
		    call sgrnd(4357)
		! a default initial seed is used
		  end if 
		  do  kk = 0, n-m-1
		    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
		    mt(kk) = IEOR(IEOR(mt(kk+m), ISHFT(y,-1)),mag01(IAND(y,1)))
		  end do
		  do  kk = n-m, n-2
		    y = IOR(IAND(mt(kk),umask), IAND(mt(kk+1),lmask))
		    mt(kk) = IEOR(IEOR(mt(kk+(m-n)), ISHFT(y,-1)),mag01(IAND(y,1)))
		  end do
		  y = IOR(IAND(mt(n-1),umask), IAND(mt(0),lmask))
		  mt(n-1) = IEOR(IEOR(mt(m-1), ISHFT(y,-1)),mag01(IAND(y,1)))
		  mti = 0
		end if
		y = mt(mti)
		mti = mti + 1
		y = IEOR(y, tshftu(y))
		y = IEOR(y, IAND(tshfts(y),tmaskb))
		y = IEOR(y, IAND(tshftt(y),tmaskc))
		y = IEOR(y, tshftl(y))
		if(y < 0) then
		  fn_val = (DBLE(y) + 2.0D0**32) / (2.0D0**32 - 1.0D0)
		else
		  fn_val = DBLE(y) / (2.0D0**32 - 1.0D0)
		end if
		return
	end function grnd
!***********************************************************************
	function tshftu(y) result(fn_val)
		integer, intent(in) :: y
		integer             :: fn_val
		fn_val = ISHFT(y,-11)
		return
	end function tshftu
!***********************************************************************
	function tshfts(y) result(fn_val)
		integer, intent(in) :: y
		integer             :: fn_val
		fn_val = ISHFT(y,7)
		return
	end function tshfts
!***********************************************************************
	function tshftt(y) result(fn_val)
		integer, intent(in) :: y
		integer             :: fn_val
		fn_val = ISHFT(y,15)
		return
	end function tshftt
!***********************************************************************
	function tshftl(y) result(fn_val)
		integer, intent(in) :: y
		integer             :: fn_val
		fn_val = ISHFT(y,-18)
		return
	end function tshftl
!***********************************************************************
end module mersenne_twister