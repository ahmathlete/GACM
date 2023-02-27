module GACM
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, GACM!"
  end subroutine say_hello
end module GACM
