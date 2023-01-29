module lexicon
    implicit none
    
    character (len=15), allocatable :: hashTable(:)
    character (len=15) :: dictWord
    integer :: capacity = 0

    contains
    subroutine hashfunction(dictWord, hash)
        character (len=15), intent(in) :: dictWord
        integer, intent(out) :: hash

        integer :: i
        integer :: charNum

        do i = 1, 15
            if (dictWord(i:i).NE." ") then
                charNum = iachar(dictWord(i:i))
                hash = hash * 33
                hash = hash + charNum
                hash = modulo(hash, capacity)
            end if
        end do
    end subroutine hashfunction

    subroutine buildlexicon(fileError)
        implicit none

        integer, intent(out) :: fileError
        character (len=50) :: fname
        logical :: lexist
        integer :: numProbed = 0
        integer :: index = 0
        integer :: hash = 0
        integer :: i = 0

        write(*, fmt="(A20)") "Enter the file name:"

        read (*,'(A)') fname

        inquire(file=fname, exist=lexist)

        if (lexist) then
            open(unit=9, file=fname, action='read')

            DO WHILE(1.EQ.1)
                READ(9, *, END=100) dictWord
                capacity = capacity + 1
            END DO

            100 CONTINUE

            close (9)

            allocate (hashTable(capacity))

            do i = 1, capacity
                hashTable(i) = ""
            end do

            open(unit=9, file=fname, action='read')

            DO WHILE(1.EQ.1)
                numProbed = 0
                READ(9, *, END=200) dictWord

                ! hash
                hash = 5381
                call hashfunction(dictWord, hash)
                
                index = hash

                do while(numProbed.LT.capacity)
                    if (hashTable(index).EQ."") then
                        hashTable(index) = dictWord
                        exit
                    end if

                    index = index + 1
                    index = modulo(index, capacity)

                    numProbed = numProbed + 1
                end do
            END DO

            200 CONTINUE
        else
            write(*, fmt="(a)") trim('File does not exist. Aborting...')
            fileError = 1
            return
        end if

        close (9)

        return
    end subroutine buildlexicon

    subroutine findlex(searchWord, wordFound, seen, seenLen)
        implicit none

        character (len=15), intent(in) :: searchWord
        integer, intent(out) :: wordFound
        character (len=15), intent(out) :: seen(1000)
        integer, intent(out) :: seenLen

        integer :: numProbed
        integer :: index
        integer :: hash
        integer :: wasSeen
        integer :: i

        wasSeen = 0

        ! hash
        hash = 5381
        call hashfunction(dictWord, hash)

        index = hash
        numProbed = 0

        do i = 1, seenLen
            if (seen(i) == searchWord) then
                wasSeen = 1
                exit
            end if
        end do

        if (wasSeen.EQ.1) then
            return
        end if

        do while(numProbed.LT.capacity)
            if (hashTable(index).NE."") then
                if (hashTable(index) == searchWord) then
                    wordFound = 1
                    seen(seenLen+1) = searchWord
                    seenLen = seenLen + 1

                    exit
                end if
            else
                wordFound = 0
                exit
            end if

            index = index + 1
            index = modulo(index, capacity)

            numProbed = numProbed + 1
        end do

        return
    end subroutine findlex
end module lexicon
