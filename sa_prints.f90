module sa_prints
implicit none
contains
	subroutine prt1()
		!  This subroutine prints intermediate output, as does PRT2 through
		!  PRT10. Note that if SA is minimizing the function, the sign of the
		!  function value and the directions (up/down) are reversed in all
		!  output to correspond with the actual function optimization. This
		!  correction is because SA was written to maximize functions and
		!  it minimizes by maximizing the negative a function.
		write(*, '(/, "  THE STARTING VALUE (X) IS OUTSIDE THE BOUNDS "/,  &
		      &     "  (lb AND ub). execution terminated without any"/,  &
		      &     "  optimization. respecify x, ub OR lb so that  "/,  &
		      &     "  lb(i) < x(i) < ub(i), i = 1, n. "/)')
		return
	end subroutine prt1
!***********************************************************************	
	subroutine prt2(max, n, x, f)
		real, intent(in) ::  x(:), f
		integer, intent(in)   ::  n
		logical, intent(in)   ::  max
		write(*, '("  ")')
		call prtvec(x,n,'INITIAL X')
		if (max) then
		  write(*, '("  INITIAL F: ",/, G25.18)') f
		ELSE
		  write(*, '("  INITIAL F: ",/, G25.18)') -f
		end if	
		return
	end subroutine prt2
!***********************************************************************	
	subroutine prt3(max, n, xp, x, f)
		real, intent(in) ::  xp(:), x(:), f
		integer, intent(in)   ::  n
		logical, intent(in)   ::  max
		write(*, '("  ")')
		call prtvec(x, n, 'CURRENT X')
		if (max) then
		  write(*, '("  CURRENT F: ", G25.18)') f
		ELSE
		  write(*, '("  CURRENT F: ", G25.18)') -f
		end if
		call prtvec(xp, n, 'TRIAL X')
		write(*, '("  POINT REJECTED SINCE OUT OF BOUNDS")')
		return
	end subroutine prt3
!***********************************************************************	
	subroutine prt4(max, n, xp, x, fp, f)
		real, intent(in) ::  xp(:), x(:), fp, f
		integer, intent(in)   ::  n
		logical, intent(in)   ::  max
		write(*,'("  ")')
		call prtvec(x,n,'CURRENT X')
		if (max) then
		  write(*,'("  CURRENT F: ",G25.18)') f
		  call prtvec(xp,n,'TRIAL X')
		  write(*,'("  RESULTING F: ",G25.18)') fp
		ELSE
		  write(*,'("  CURRENT F: ",G25.18)') -f
		  call prtvec(xp,n,'TRIAL X')
		  write(*,'("  RESULTING F: ",G25.18)') -fp
		end if
		return
	end subroutine prt4
!***********************************************************************	
	subroutine prt5()
		write(*, '(/, "  TOO MANY FUNCTION EVALUATIONS; CONSIDER "/,  &
		      &     "  increasing maxevl OR eps, OR decreasing "/,  &
		      &     "  nt OR rt. these results are likely TO be "/, "  poor.",/)')
		return
	end subroutine prt5
!***********************************************************************	
	subroutine prt6(max)
		logical, intent(in) ::  max
		if (max) then
		  write(*,'("  THOUGH LOWER, POINT ACCEPTED")')
		ELSE
		  write(*,'("  THOUGH HIGHER, POINT ACCEPTED")')
		end if
		return
	end subroutine prt6
!***********************************************************************	
	subroutine prt7(max)
		logical, intent(in) :: max
		if (max) then
		  write(*,'("  LOWER POINT REJECTED")')
		ELSE
		  write(*,'("  HIGHER POINT REJECTED")')
		end if
		return
	end subroutine prt7
!***********************************************************************	
	subroutine prt8(n, vm, xopt, x)
		real, intent(in) :: vm(:), xopt(:), x(:)
		integer, intent(in)   :: n
		write(*,'(/, " intermediate results after step length adjustment", /)')
		call prtvec(vm, n, 'NEW STEP LENGTH (VM)')
		call prtvec(xopt, n, 'CURRENT OPTIMAL X')
		call prtvec(x, n, 'CURRENT X')
		write(*,'(" ")')
		return
	end subroutine prt8
!***********************************************************************	
	subroutine prt9(max, n, t, xopt, vm, fopt, nup, ndown, nrej, lnobds, nnew)
		real, intent(in) :: xopt(:), vm(:), t, fopt
		integer, intent(in)   :: n, nup, ndown, nrej, lnobds, nnew
		logical, intent(in)   :: max
		! Local variable
		integer :: totmov
		totmov = nup + ndown + nrej
		write(*,'(/," intermediate results before next temperature reduction",/)')
		write(*,'("  CURRENT TEMPERATURE:            ",G12.5)') t
		if (max) then
		  write(*, '("  MAX FUNCTION VALUE SO FAR:  ",G25.18)') fopt
		  write(*, '("  TOTAL MOVES:                ",I8)') totmov
		  write(*, '("     UPHILL:                  ",I8)') nup
		  write(*, '("     ACCEPTED DOWNHILL:       ",I8)') ndown
		  write(*, '("     REJECTED DOWNHILL:       ",I8)') nrej
		  write(*, '("  OUT OF BOUNDS TRIALS:       ",I8)') lnobds
		  write(*, '("  NEW MAXIMA THIS TEMPERATURE:",I8)') nnew
		ELSE
		  write(*, '("  MIN FUNCTION VALUE SO FAR:  ",G25.18)') -fopt
		  write(*, '("  TOTAL MOVES:                ",I8)') totmov
		  write(*, '("     DOWNHILL:                ",I8)')  nup
		  write(*, '("     ACCEPTED UPHILL:         ",I8)')  ndown
		  write(*, '("     REJECTED UPHILL:         ",I8)')  nrej
		  write(*, '("  TRIALS OUT OF BOUNDS:       ",I8)')  lnobds
		  write(*, '("  NEW MINIMA THIS TEMPERATURE:",I8)')  nnew
		end if
		call prtvec(xopt, n, 'CURRENT OPTIMAL X')
		call prtvec(vm, n, 'STEP LENGTH (VM)')
		write(*, '(" ")')
		return
	end subroutine prt9
!***********************************************************************	
	subroutine prt10()
		write(*, '(/, "  SA ACHIEVED TERMINATION CRITERIA. IER = 0. ",/)')
		return
	end subroutine prt10
!***********************************************************************	
	subroutine prtvec(vector, ncols, name)
		!  This subroutine prints the double precision vector named VECTOR.
		!  Elements 1 thru NCOLS will be printed. NAME is a character variable
		!  that describes VECTOR. Note that if NAME is given in the call to
		!  PRTVEC, it must be enclosed in quotes. if there are more than 10
		!  elements in VECTOR, 10 elements will be printed on each line.
		integer, intent(in)           :: ncols
		real, intent(in)         :: vector(ncols)
		CHARACTER (LEN=*), intent(in) :: name
		integer :: i, lines, ll
		write(*,1001) NAME
		if (ncols > 10) then
		  lines = INT(ncols/10.)
		  DO i = 1, lines
		    ll = 10*(i - 1)
		    write(*,1000) vector(1+ll:10+ll)
		  end DO
		  write(*,1000) vector(11+ll:ncols)
		ELSE
		  write(*,1000) vector(1:ncols)
		end if
		1000 FORMAT( 10(g12.5, ' '))
		1001 FORMAT(/, 25(' '), a)
		return
	end subroutine prtvec
!***********************************************************************
end module sa_prints