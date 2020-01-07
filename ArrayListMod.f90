module ArrayListMod
    implicit none

    integer,parameter::dp = 8
    
    type ArrayListElement
        real(kind=dp)::value
        type(ArrayListElement),pointer::previous=>null()
        type(ArrayListElement),pointer::next =>null()
    end type

    type ArrayList
        type(ArrayListElement),public,pointer::head=>null()
        type(ArrayListElement),public,pointer::tail=>null()
        type(ArrayListElement),public,pointer::iterator
    contains
        procedure :: add => add_
        procedure :: addTop => addTop_
        procedure :: get => get_
        procedure :: pop => pop_
        procedure :: popLast => popLast_
        ! procedure :: getItr => getItr_
        ! procedure :: erase => erase_
        ! procedure :: getHeadPtr => getHeadPtr_
        ! procedure :: getTailPtr => getTailPtr_
        procedure :: getAllValue => getAllValue_
        procedure :: length => length_
    end type

    type(ArrayListElement),pointer:: dummy

    interface ArrayList
        module procedure initialize
    end interface

    contains

    type(ArrayList) function initialize(self, tag_)
        implicit none
        type(ArrayList)::self
        character(len=*)::tag_
        self%tail=>self%head
    end function initialize

    subroutine add_(self, val_)
        class(ArrayList),intent(inout)::self
        real(kind=dp),intent(in)::val_
        
        ! case of top
        if(associated(self%head).eqv. .false.)then
            allocate(dummy)
            
            self%head => dummy
            self%head%value = val_
            self%tail => self%head
            dummy => null()
        else
            allocate(dummy)
            dummy%value = val_
            self%tail%next => dummy 
            dummy%previous => self%tail
            self%tail      => dummy
            dummy          => null()
        end if

    end subroutine

    subroutine addTop_(self, val_)
        class(ArrayList),intent(inout)::self
        complex(kind=dp),intent(in)::val_
        
        ! case of top
        if(associated(self%head).eqv. .false.)then
            allocate(dummy)
            self%head => dummy
            self%head%value = val_
            self%tail => self%head
            dummy => null()
        else
            
            allocate(dummy)
            dummy%value = val_

            dummy%next => self%head
            self%head => dummy
            dummy%previous => null()
            dummy          => null()
        end if

    end subroutine

    subroutine addTag(self,tag_)
        implicit none
        class(ArrayList) self
        character(len=100) tag_
    end subroutine addTag

    function get_(self, index) result(res)
        implicit none
        class(ArrayList),intent(inout)::self
        integer itr
        type(ArrayListElement),pointer::ptr
        integer,intent(in):: index
        real(kind=dp)::res

        ptr => self%head
        if(index==1) then
            res = ptr%value
            return
        end if

        do itr=1,index-1
            if(associated(ptr%next)) then
                ptr => ptr%next
            else
                print *, "STOP[] this pointer reffered null pointer"
                stop
            end if
        end do

        res = ptr%value
    end function

    subroutine pop_(self)
        implicit none
        class(ArrayList) self
        type(ArrayListElement),pointer::ptr
        ptr=>self%head%next
        deallocate(self%head)
        self%head => ptr
        self%head%previous => null()
    end subroutine

    subroutine popLast_(self)
        implicit none
        class(ArrayList) self
        type(ArrayListElement),pointer::ptr
        integer error
        ptr=>self%tail
        self%tail => self%tail%previous
        self%tail%next => null()
        deallocate(ptr)

    end subroutine

!NOTE: These methods are not still implemeted.

    ! subroutine erase_(self, index)
    !     implicit none
    !     class(ArrayList) self
    !     integer,intent(in)::index
    !     type(ArrayListElement),pointer::ptr, pre_ptr
    !     integer itr, error

    !     print *,"This subroutine is not working"
    !     return

    !     if(index==1) then
    !         call self%pop()
    !         return
    !     end if

    !     ptr => self%head
        
    !     do itr=1,index-1
    !         if(associated(ptr%next)) then
    !             ptr => ptr%next
    !         else
    !             stop "this pointer reffered null pointer"
    !         end if
    !     end do

    !     pre_ptr => ptr%previous
    !     pre_ptr%next => ptr%next
    !     ! print *,associated(ptr)
    !     deallocate(ptr,stat=error)

    ! end subroutine
    
    ! subroutine getItr_(self, res)
    !     implicit none
    !     class(ArrayList)::self
    !     real(kind=dp) res
    !     res = self%iterator%value
    ! end subroutine

    subroutine getAllValue_(self,res)
        implicit none
        class(ArrayList) self
        real(kind=dp),pointer::res(:)
        type(ArrayListElement),pointer::ptr
        integer list_count, itr

        if(associated(self%head) .eqv. .false.) then
            stop "[ArrayList->getAllValue] This data structure has no data"
        end if

        ptr => self%head
        list_count = 1
        do while(loc(ptr)/=loc(self%tail))
            ptr => ptr%next
            list_count =list_count + 1
        end do

        ptr => self%head
        allocate(res(list_count))
        
        do itr = 1, list_count
            res(itr) = ptr%value
            ptr => ptr%next
        end do
    end subroutine

    ! subroutine getHeadPtr_(self, ptr)
    !     implicit none
    !     class(ArrayList) self
    !     type(ArrayListElement),pointer::ptr

    !     ptr => self%head
    ! end subroutine

    ! subroutine getTailPtr_(self,ptr)
    !     implicit none
    !     class(ArrayList) self
    !     type(ArrayListElement),pointer::ptr

    !     ptr => self%tail
    ! end subroutine

    function length_(self) result(res)
        implicit none
        class(ArrayList) self
        integer::res
        type(ArrayListElement),pointer::ptr

        ptr => self%head
        res = 1
        do while(loc(ptr)/=loc(self%tail))
            ptr => ptr%next
            res = res + 1
        end do

    end function

end module

! program test
!     use ArrayListMod
!     type(ArrayList) a
!     type(ArrayListElement),pointer:: c
!     complex(kind=dp) b(2)
!     real(kind=dp) ,pointer::d(:,:)
!     character(len=100) cc
!     cc="bbbbb"

!     a = initialize(a,2,cc)
!     b = [1,2]
!     call a%add(b)
!     b=[2,4]
!     call a%add(b)
!     b=[3,4]
!     call a%add(b)
!     print*,a%get(4)
!     call a%addTop(b)
!     call a%popLast()
!     print *, a%length()
!     call a%getAllValue(d)
!     print *, d
! end program