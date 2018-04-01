program main
  use simann
  use sa_prints
  implicit none
  integer, parameter :: n = 2, neps = 4
  real   :: lb(n), ub(n), x(n), xopt(n), c(n), vm(n), t, eps, rt, fopt
  integer     :: ns, nt, nfcnev, ier, i, maxevl, iprint,  &
                 &nacc, nobds
  logical     :: max
  max = .false.
  eps = 1.0D-6
  rt = .5
  ns = 1000
  nt = 1000
  maxevl = 10000000
  iprint = 1
  do i = 1, n
    lb(i) = -1.0D25
    ub(i) =  1.0D25
    c(i) = 2.0
  end do
  !  Note start at local, but not global, optima of the Judge function.
  x(1) =  2.354471
  x(2) = -0.319186
  !  Set input values of the input/output parameters.
  t = 5.0
  vm(1:n) = 1.0
  write(*,1000) n, max, t, rt, eps, ns, nt, neps, maxevl, iprint
  call prtvec(x, n, 'STARTING VALUES')
  call prtvec(vm, n, 'INITIAL STEP LENGTH')
  call prtvec(lb, n, 'LOWER BOUND')
  call prtvec(ub, n, 'UPPER BOUND')
  call prtvec(c, n, 'C VECTOR')
  write(*, '(/, "  ****   END OF DRIVER ROUTINE OUTPUT   ****"/,  &
        &     "  ****   before CALL TO sa.             ****")')
  call sa(n, x, max, rt, eps, ns, nt, neps, maxevl, lb, ub, c, iprint,&
          &t, vm, xopt, fopt, nacc, nfcnev, nobds, ier)
  write(*, '(/, "  ****   RESULTS AFTER SA   ****   ")')
  call prtvec(xopt, n, 'SOLUTION')
  call prtvec(vm, n, 'FINAL STEP LENGTH')
  write(*,1001) fopt, nfcnev, nacc, nobds, t, ier
  1000 format(/,' SIMULATED ANNEALING EXAMPLE',/,/,  &
               ' NUMBER OF PARAMETERS: ',i3,'   MAXIMIZATION: ',l5, /, &
               ' INITIAL TEMP: ', g10.2, '   RT: ',g10.2, '   EPS: ',g10.2, /, &
               ' NS: ',i3, '   NT: ',i2, '   NEPS: ',i2, /,  &
               ' MAXEVL: ',i10, '   IPRINT: ',i1)
  1001 format(/,' OPTIMAL FUNCTION VALUE: ',g20.13  &
              /,' NUMBER OF FUNCTION EVALUATIONS:     ',i10,  &
              /,' NUMBER OF ACCEPTED EVALUATIONS:     ',i10,  &
              /,' NUMBER OF OUT OF BOUND EVALUATIONS: ',i10,  &
              /,' FINAL TEMP: ', g20.13,'  IER: ', i3)
end program main