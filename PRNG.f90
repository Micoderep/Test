Program RNG
  implicit none
  real :: xseed = 1, x1, x2, M = 128, a=13, c = 43, mean = 0, sqmean = 0, var
  integer :: i, imax = 100

  x1 = xseed
   
  open(unit=11, file ="rngtest.txt")

  do i = 1,imax

    x2 = mod(a*x1 + c, M)
    x1 = x2
    
    !if (x2 == xseed) then
    !  print*, i
    !end if 

    write(unit=11,fmt=*) x2/M, i/real(imax)

    !!!!!!!!!!!!!!!!
    mean = mean + x2/M
    sqmean = sqmean + (x2/M)**2
    !!!!!!!!!!!!
  end do

  close(unit=11) 

  mean = mean/imax
  sqmean = sqmean/imax

  var = sqmean - mean**2

  print*, mean, sqmean, var

  qjhbKIQD
  

End Program RNG

