module test_fcn
implicit none
contains
	subroutine fcn(n, theta, h)
		!  This subroutine is from the example in Judge et al., The Theory and
		!  Practice of Econometrics, 2nd ed., pp. 956-7. There are two optima:
		!  F(.864,1.23) = 16.0817 (the global minumum) and F(2.35,-.319) = 20.9805.
		integer, intent(in)    :: n
		real, intent(in)  :: theta(:)
		real, intent(out) :: h
		! Local variables
		integer   :: i
		real :: y(20), x2(20), x3(20)
		y(1) = 4.284
		y(2) = 4.149
		y(3) = 3.877
		y(4) = 0.533
		y(5) = 2.211
		y(6) = 2.389
		y(7) = 2.145
		y(8) = 3.231
		y(9) = 1.998
		y(10) = 1.379
		y(11) = 2.106
		y(12) = 1.428
		y(13) = 1.011
		y(14) = 2.179
		y(15) = 2.858
		y(16) = 1.388
		y(17) = 1.651
		y(18) = 1.593
		y(19) = 1.046
		y(20) = 2.152
		
		x2(1) =  .286
		x2(2) =  .973
		x2(3) =  .384
		x2(4) =  .276
		x2(5) =  .973
		x2(6) =  .543
		x2(7) =  .957
		x2(8) =  .948
		x2(9) =  .543
		x2(10) =  .797
		x2(11) =  .936
		x2(12) =  .889
		x2(13) =  .006
		x2(14) =  .828
		x2(15) =  .399
		x2(16) =  .617
		x2(17) =  .939
		x2(18) =  .784
		x2(19) =  .072
		x2(20) =  .889
		
		x3(1) = .645
		x3(2) = .585
		x3(3) = .310
		x3(4) = .058
		x3(5) = .455
		x3(6) = .779
		x3(7) = .259
		x3(8) = .202
		x3(9) = .028
		x3(10) = .099
		x3(11) = .142
		x3(12) = .296
		x3(13) = .175
		x3(14) = .180
		x3(15) = .842
		x3(16) = .039
		x3(17) = .103
		x3(18) = .620
		x3(19) = .158
		x3(20) = .704
		h = 0.
		do i = 1, 20
		  h = (theta(1) + theta(n)*x2(i) + (theta(n)**2)*x3(i) - y(i))**2 + h
		end do
	return
	end subroutine fcn
end module test_fcn