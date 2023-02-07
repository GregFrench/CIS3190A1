! File name: lexicon.f95
! Author: Greg French (1084574)
! Date last modified: February 06, 2023
! Purpose: A module that provides helper subroutines for the solvejumble program

module lexicon
    implicit none
    
    ! Declare local module variables
    character (len=15), allocatable :: hashTable(:)
    character (len=15) :: dictWord
    integer :: capacity = 0

    contains
    ! Convert an uppercase character to lowercase 
    subroutine to_lower(c)
        implicit none

        character, intent(out) :: c
        integer :: charAscii
        
        charAscii = ichar(c)
        
        if (charAscii >= 65.and.charAscii <= 90) then
            c = char(charAscii+32)
        end if
    end subroutine to_lower

    ! String hash function for storing and retrieving words from the hash table
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

    ! intializes the hash table with words from the input dictionary
    subroutine buildlexicon(fileError)
        implicit none

        ! Declare calling parameter types and definitions
        integer, intent(out) :: fileError

        ! Declare local variables
        logical :: lexist
        integer :: numProbed = 0
        integer :: index = 0
        integer :: hash = 0
        integer :: i = 0
        integer :: j = 0

        inquire(file='dict2.txt', exist=lexist)

        ! check if the file exists
        if (lexist) then
            open(unit=9, file='dict2.txt', action='read')

            ! calculate the total number of lines in the file
            DO WHILE(1.EQ.1)
                READ(9, *, END=100) dictWord
                capacity = capacity + 1
            END DO

            100 CONTINUE

            close (9)

            ! allocate enough memory to store all the words
            allocate (hashTable(capacity+5))

            do i = 1, capacity
                hashTable(i) = ""
            end do

            open(unit=9, file='dict2.txt', action='read')

            ! store all words from the dictionary using linear probing to find open spots
            DO WHILE(1.EQ.1)
                numProbed = 0
                READ(9, *, END=200) dictWord

                do j = 1, 15
                    call to_lower(dictWord(j:j))
                end do

                ! call the hash function on the word
                hash = 5381
                call hashfunction(dictWord, hash)
                
                index = hash

                ! find an empty spot in the hash table starting at the
                ! hashed index location for which to store the dictionary word
                do while(numProbed.LT.capacity)
                    if (index.EQ.0) then
                        index = index + 1
                    end if

                    if (hashTable(index).EQ."") then
                        hashTable(index) = dictWord
                        exit
                    end if

                    index = index + 1
                    index = modulo(index, capacity+1)

                    numProbed = numProbed + 1
                end do
            END DO

            200 CONTINUE
        else
            ! terminate program if an error occurs opening the file
            write(*, fmt="(a)") trim('File does not exist. Aborting...')
            fileError = 1
            return
        end if

        close (9)

        return
    end subroutine buildlexicon

    ! find if a word exists in the hash table
    subroutine findlex(searchWord, wordFound, wasSeen, seen, seenLen)
        implicit none

        ! Declare calling parameter types and definitions
        character (len=15), intent(in) :: searchWord
        integer, intent(out) :: wordFound
        integer, intent(out) :: wasSeen
        character (len=15), intent(out) :: seen(1000)
        integer, intent(out) :: seenLen

        ! Declare local variables
        integer :: numProbed
        integer :: index
        integer :: hash
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

        ! linear probing on the hash table to find if word exists
        do while(numProbed.LT.capacity)
            if (index.EQ.0) then
                index = index + 1
            end if

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
            index = modulo(index, capacity+1)

            numProbed = numProbed + 1
        end do

        return
    end subroutine findlex
end module lexicon
