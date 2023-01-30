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

! Convert a lowercase character to uppercase 
subroutine to_upper(c)
    implicit none

    character, intent(out) :: c
    integer :: charAscii
    
    charAscii = ichar(c)
    
    if (charAscii >= 97.and.charAscii <= 122) then
        c = char(charAscii-32)
    end if
end subroutine to_upper

! Gets the length of a word
subroutine wordLen(word, len)
    implicit none

    character (len=15), intent(in) :: word
    integer, intent(out) :: len
    integer :: i
    
    do i = 1, 15
        if (word(i:i).NE." ") then
            len = len + 1
        end if
    end do
end subroutine wordLen

subroutine inputJumble(numJumbledWords, words)
    implicit none

    ! Declare calling parameter types and definitions
    integer, intent(out) :: numJumbledWords
    character (len=15), intent(out) :: words(100)

    ! Declare local variables
    integer :: i
    integer :: j
    character (len=15) :: str

    write(*, fmt="(A24)", advance="no") "How many jumbled words? "

    read (*,*) numJumbledWords

    print '(A1)', ' '

    print '(A10, I0, :, A15)', 'Enter the ', numJumbledWords, ' jumbled words:'

    do i = 1, numJumbledWords
        write(*, fmt="(A2)", advance="no") "> "
        read (*,*) str

        do j = 1, 15
            call to_lower(str(j:j))
        end do

        words(i) = str
    end do

    print '(A1)', ' '

    return
end subroutine inputJumble

recursive subroutine generateAnagram(word, wordSize, curr, currSize, wordList, wordListSize, chosen)
    ! Declare calling parameter types and definitions
    character (len=15), intent(in) :: word
    integer, intent(in) :: wordSize
    character (len=15), intent(out) :: curr
    integer, intent(out) :: currSize
    character (len=15), intent(out) :: wordList(10000)
    integer, intent(out) :: wordListSize
    integer, intent(out) :: chosen(15)

    ! Declare local variables
    integer :: i

    if (wordSize.EQ.currSize) THEN
        wordList(wordListSize+1) = curr
        wordListSize = wordListSize + 1
        return
    end if

    do i = 1, wordSize
        if (chosen(i).EQ.0) THEN
            chosen(i) = 1
            curr(currSize+1:currSize+1) = word(i:i)
            currSize = currSize + 1
            call generateAnagram(word, wordSize, curr, currSize, wordList, wordListSize, chosen)
            currSize = currSize - 1
            chosen(i) = 0
        end if
    end do
end subroutine generateAnagram

subroutine findAnagram(wordList, wordListSize, wordResList, wordResListSize)
    use lexicon

    character (len=15), intent(in) :: wordList(10000)
    integer, intent(in) :: wordListSize
    character (len=15), intent(out) :: wordResList(2000)
    integer, intent(out) :: wordResListSize
    character (len=15) :: seen(1000)
    integer :: i
    integer :: wordFound
    integer :: seenLen

    wordFound = 0
    seenLen = 0

    do i = 1, wordListSize
        call findlex(wordList(i), wordFound, seen, seenLen)

        if (wordFound.EQ.1) then
            if (wasSeen.EQ.0) then
                wordResList(wordResListSize+1) = wordList(i)
                wordResListSize = wordResListSize + 1

                write(*, fmt="(A15)", advance="no") wordList(i)
            end if
        end if

        wasSeen = 0
        wordFound = 0
    end do

    print '(A1)', ' '
end subroutine findAnagram

program solvejumble
    use lexicon

    integer numJumbledWords
    character (len=15) :: words(100)
    character (len=15) :: word
    integer :: wordSize
    character (len=15) :: curr
    integer :: currSize
    character (len=15), allocatable :: wordList(:)
    integer :: wordListSize
    character (len=15) :: wordResList(2000)
    integer :: wordResListSize
    integer :: chosen(15)
    integer :: i
    integer :: j
    character (len=15) :: str
    character (len=15) :: jumbledWord
    integer :: jumbledWordSize
    integer :: fileError
    integer :: len
    integer :: longestWord
    integer :: factorial

    jumbledWord = ""
    jumbledWordSize = 0
    wordResListSize = 0
    fileError = 0
    len = 0
    longestWord = 0
    factorial = 1

    call buildlexicon(fileError)

    if (fileError.EQ.1) then
        return
    end if

    call inputJumble(numJumbledWords, words)

    do i = 1, numJumbledWords
        call wordLen(words(i), len)

        if (len.GT.longestWord) then
            longestWord = len
        end if
    end do

    do i = 1, longestWord
        factorial = i * factorial
    end do

    allocate (wordList(factorial))

    write(*, fmt="(A39)") "The following jumbles have been solved:"

    do i = 1, 15
        chosen(i) = 0
    end do

    do i = 1, numJumbledWords
        word = words(i)
        wordSize = 0
        currSize = 0
        wordListSize = 0
        curr = ''

        do j = 1, 15
            if (word(j:j).NE." ") then
                wordSize = wordSize + 1
            end if
        end do

        do j = 1, 15
            call to_upper(word(j:j))
        end do

        write(*, fmt="(A15)", advance="no") word

        do j = 1, 15
            call to_lower(word(j:j))
        end do

        call generateAnagram(word, wordSize, curr, currSize, wordList, wordListSize, chosen)
        call findAnagram(wordList, wordListSize, wordResList, wordResListSize)
    end do

    print '(A1)', ' '

    write(*, fmt="(A40)") "Select circled letters from word puzzle:"

    do i = 1, wordResListSize
        write(*, fmt="(a)", advance="no") trim(wordResList(i))
        write(*, fmt="(A2)", advance="no") ': '
        read (*, '(a)') str

        do j = 1, 15
            if (str(j:j).NE." ") then
                jumbledWord(jumbledWordSize+1:jumbledWordSize+1) = str(j:j)
                jumbledWordSize = jumbledWordSize + 1
            end if
        end do
            
    end do
    
    write(*, fmt="(a)", advance="no") "Jumbled word: "

    write(*, fmt="(a)") trim(jumbledWord)

    print '(A1)', ' '

    write(*, fmt="(a)", advance="no") "Solved jumble: "

    word = jumbledWord
    wordSize = jumbledWordSize
    currSize = 0
    wordListSize = 0
    wordResListSize = 0
    curr = ''

    call generateAnagram(word, wordSize, curr, currSize, wordList, wordListSize, chosen)
    call findAnagram(wordList, wordListSize, wordResList, wordResListSize)

    deallocate (wordList)

end program solvejumble
