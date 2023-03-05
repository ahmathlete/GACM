module GACM_PRESSURE
   use, intrinsic :: iso_c_binding
   implicit none
   private
   public :: GACM_PRESSURE_HYBRID_SIGMA
   public :: GACM_PRESSURE_HYBRID_SIGMA_HEIGHT
contains
   subroutine GACM_PRESSURE_HYBRID_SIGMA(nlat, nlon, nlevel, ntime, &
                                         surface_pressure, a, b, p0, &
                                         PRESSURE_4D) bind(C, name="GACM_PRESSURE_HYBRID_SIGMA_")

      integer(c_int), value                                             :: nlat, nlon, nlevel, ntime
      real(c_double)                                                    :: surface_pressure(nlat, nlon, ntime)
      real(c_double)                                                    :: a(nlevel), b(nlevel), p0
      real(c_double), intent(out)                                       :: PRESSURE_4D(nlat, nlon, nlevel, ntime)
      integer(c_int)                                                    :: i, j, l, t

      ! can be parralised with omp
      do t = 1, ntime
         do l = 1, nlevel
            do j = 1, nlon
               do i = 1, nlat
                  PRESSURE_4D(i, j, l, t) = a(l)*p0 + b(l)*surface_pressure(i, j, t)
               end do
            end do
         end do
      end do

   end subroutine GACM_PRESSURE_HYBRID_SIGMA
   subroutine GACM_PRESSURE_HYBRID_SIGMA_HEIGHT() bind(C, name="GACM_PRESSURE_HYBRID_SIGMA_HEIGHT_")

   end subroutine GACM_PRESSURE_HYBRID_SIGMA_HEIGHT
end module GACM_PRESSURE
