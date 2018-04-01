module simann
use mersenne_twister
use sa_prints
use test_fcn
implicit none
contains
  subroutine sa(n, x, max, rt, eps, ns, nt, neps, maxevl, lb, ub, c, iprint, &
                &t, vm, xopt, fopt, nacc, nfcnev, nobds, ier)
    real, intent(in):: lb(:), ub(:), c(:), eps, rt
    real, intent(inout):: x(:), t, vm(:)
    real, intent(out):: xopt(:), fopt
    integer, intent(in):: n, ns, nt, neps, maxevl, iprint
    integer, intent(out):: nacc, nfcnev, nobds, ier
    LOGICAL, intent(in):: max
    !  Type all internal variables.
    real :: f, fp, p, pp, ratio, xp(n), fstar(neps)
    integer:: nup, ndown, nrej, nnew, lnobds, h, i, j, m, nacp(n)
    logical:: quit,finish_criteria
    !for mersenne twister
    integer::clock,seed
    !  Initialize the random number generator.
    finish_criteria = .false.
    call SYSTEM_CLOCK(COUNT=clock)
    seed = clock
    call init_genrand(seed)
    !  Set initial values.
    nacc = 0
    nobds = 0
    nfcnev = 0
    ier = 99
    do i = 1, n
      xopt(i) = x(i)
      nacp(i) = 0
    end do
    fstar = 1.0D+20
    !  if the initial temperature is not positive, notify the user and
    !  return to the calling routine.
    if (t <= 0.0) then
      write(*,'(/, "  THE INITIAL TEMPERATURE IS NOT POSITIVE. "/,  &
            &    "  reset the variable t. "/)')
      ier = 3
      return
    end if
    !  if the initial value is out of bounds, notify the user and return
    !  to the calling routine.
    do i = 1, n
      if ((x(i) > ub(i)) .OR. (x(i) < lb(i))) then
        call prt1()
        ier = 2
        return
      end if
    end do
    !  Evaluate the function with input X and return value as F.
    call fcn(n, x, f)
    !  if the function is to be minimized, switch the sign of the function.
    !  Note that all intermediate and final output switches the sign back
    !  to eliminate any possible confusion for the user.
    if(.NOT. max) f = -f
    nfcnev = nfcnev + 1
    fopt = f
    fstar(1) = f
    if(iprint >= 1) call prt2(max, n, x, f)
    !  Start the main loop. Note that it terminates if (i) the algorithm
    !  succesfully optimizes the function or (ii) there are too many
    !  function evaluations (more than MAXEVL).
    do while(finish_criteria .eqv. .false.)
    nup = 0
    nrej = 0
    nnew = 0
    ndown = 0
    lnobds = 0
    do m = 1, nt
      do j = 1, ns
        do h = 1, n  
    !  Generate XP, the trial value of X. Note use of VM to choose XP.
          do i = 1, n
            if (i == h) then
              xp(i) = x(i) + (grnd()*2. - 1.) * vm(i)
            else
              xp(i) = x(i)
            end if    
    !  if XP is out of bounds, select a point in bounds for the trial.
            if((xp(i) < lb(i)) .OR. (xp(i) > ub(i))) then
              xp(i) = lb(i) + (ub(i) - lb(i))*grnd()
              lnobds = lnobds + 1
              nobds = nobds + 1
              if(iprint >= 3) call prt3(max, n, xp, x, f)
            end if
          end do    
    !  Evaluate the function with the trial point XP and return as FP.
          call fcn(n, xp, fp)
          if(.NOT. max) fp = -fp
          nfcnev = nfcnev + 1
          if(iprint >= 3) call prt4(max, n, xp, x, fp, f)    
    !  if too many function evaluations occur, terminate the algorithm.
          if(nfcnev >= maxevl) then
            call prt5()
            if (.NOT. max) fopt = -fopt
            ier = 1
            return
          end if    
    !  Accept the new point if the function value increases.
          if(fp >= f) then
            if(iprint >= 3) then
              write(*,'("  POINT ACCEPTED")')
            end if
            x(1:n) = xp(1:n)
            f = fp
            nacc = nacc + 1
            nacp(h) = nacp(h) + 1
            nup = nup + 1       
    !  if greater than any other point, record as new optimum.
            if (fp > fopt) then
              if(iprint >= 3) then
                write(*,'("  NEW OPTIMUM")')
              end if
              xopt(1:n) = xp(1:n)
              fopt = fp
              nnew = nnew + 1
            end if      
    !  if the point is lower, use the Metropolis criteria to decide on
    !  acceptance or rejection.
          else
            p = exprep((fp - f)/t)
            pp = grnd()
            if (pp < p) then
              if(iprint >= 3) call prt6(max)
              x(1:n) = xp(1:n)
              f = fp
              nacc = nacc + 1
              nacp(h) = nacp(h) + 1
              ndown = ndown + 1
            else
              nrej = nrej + 1
              if(iprint >= 3) call prt7(max)
            end if
          end if
        end do
      end do 
    !  Adjust VM so that approximately half of all evaluations are accepted.
      do i = 1, n
        ratio = DBLE(nacp(i)) /DBLE(ns)
        if (ratio > .6) then
          vm(i) = vm(i)*(1. + c(i)*(ratio - .6)/.4)
        else if (ratio < .4) then
          vm(i) = vm(i)/(1. + c(i)*((.4 - ratio)/.4))
        end if
        if (vm(i) > (ub(i)-lb(i))) then
          vm(i) = ub(i) - lb(i)
        end if
      end do
      if(iprint >= 2) then
        call prt8(n, vm, xopt, x)
      end if
      nacp(1:n) = 0  
    end do
    if(iprint >= 1) then
      call prt9(max,n,t,xopt,vm,fopt,nup,ndown,nrej,lnobds,nnew)
    end if
    !  Check termination criteria.
    quit = .false.
    fstar(1) = f
    if ((fopt - fstar(1)) <= eps) quit = .true.
    do i = 1, neps
      if (ABS(f - fstar(i)) > eps) quit = .false.
    end do
    !  Terminate SA if appropriate.
    if (quit) then
      x(1:n) = xopt(1:n)
      ier = 0
      if (.NOT. max) fopt = -fopt
      if(iprint >= 1) call prt10()
      finish_criteria = .true.
      return
    end if
    !  if termination criteria is not met, prepare for another loop.
    t = rt*t
    do i = neps, 2, -1
      fstar(i) = fstar(i-1)
    end do
    f = fopt
    x(1:n) = xopt(1:n)
    !  Loop again.
    end do
  end subroutine sa
!***********************************************************************
  function exprep(rdum) result(fn_val)
    !  This function replaces exp to avoid under- and overflows and is
    !  designed for IBM 370 type machines. It may be necessary to modify
    !  it for other machines. Note that the maximum and minimum values of
    !  EXPREP are such that they has no effect on the algorithm.
    integer, parameter :: dp = SELECTED_REAL_KIND(14, 60)
    real, intent(in) :: rdum
    real(dp)         :: fn_val
    if (rdum > 174.) then
      fn_val = 3.69d+75
    else if (rdum < -180.) then
      fn_val = 0.
    else
      fn_val = EXP(rdum)
    end if
    return
  end function exprep
end module simann