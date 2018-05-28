  MODULE mGetInput
  USE mdsPostProcessor
  CONTAINS
  !----------------------------------------------------------------------------------------------------------------------------
  FUNCTION fGetRawData(tyParentTmp)

  IMPLICIT NONE
  INTEGER :: fGetRawData
  TYPE(POSTPROCESSORROOT), POINTER :: tyParentTmp
  CHARACTER(LEN=LONGWORD) :: sFileName

  !READ INTEGER FILES
  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"DIMENSIONALITY.TXT"
  tyParentTmp%tyRaw%iNOfDimensions = fGetFileInteger(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"NUMBER_OF_ELEMENTS.TXT"
  ALLOCATE(tyParentTmp%tyRaw%iNOfElements(fGetFileSize(sFileName)))

  tyParentTmp%tyRaw%iNOfElements = fGetFileInteger(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"NUMBER_OF_GRID_POINTS.TXT"
  tyParentTmp%tyRaw%iNOfGridPoints = fGetFileInteger(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"NUMBER_OF_PHASES.TXT"
  ALLOCATE(tyParentTmp%tyRaw%iNOfPhses(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%iNOfPhses = fGetFileInteger(sFileName,fGetFileSize(sFileName))

  !READ DP FILES
  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"CHEMICAL_POTENTIALS.TXT"
  ALLOCATE(tyParentTmp%tyRaw%rChemicalPotentials(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%rChemicalPotentials = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"DOMAIN_SIZE.TXT"
  tyParentTmp%tyRaw%rDomainSize = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"FINITE_VOLUME_CENTROID_COORDINATES.TXT"
  ALLOCATE(tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"MOLE_FRACTIONS.TXT"
  ALLOCATE(tyParentTmp%tyRaw%rMoleFractions(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%rMoleFractions = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"PHASE_FRACTIONS.TXT"
  ALLOCATE(tyParentTmp%tyRaw%rPhaseFractions(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%rPhaseFractions = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"TIME.TXT"
  ALLOCATE(tyParentTmp%tyRaw%rTime(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%rTime = fGetFileDoublePrecission(sFileName,fGetFileSize(sFileName))

  !READ St FILES
  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"ELEMENT_NAMES.TXT"
  ALLOCATE(tyParentTmp%tyRaw%sNamesOfElements(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%sNamesOfElements = fGetFileString(sFileName,fGetFileSize(sFileName))

  sFileName =''
  sFileName = TRIM(tyParentTmp%sInPath)//"PHASE_NAMES.TXT"
  ALLOCATE(tyParentTmp%tyRaw%sNamesOfPhases(fGetFileSize(sFileName)))
  tyParentTmp%tyRaw%sNamesOfPhases = fGetFileString(sFileName,fGetFileSize(sFileName))
  fGetRawData = 0
  ENDFUNCTION fGetRawData
  !----------------------------------------------------------------------------------------------------------------------------
  FUNCTION fGetFileDoublePrecission(sFileName,iFileSize)
  IMPLICIT NONE
  INTEGER :: iFileSize
  DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rArray, fGetFileDoublePrecission
  CHARACTER(LEN=LONGWORD) :: sFileName

  OPEN(UNIT=1,FILE = sFileName, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
  ALLOCATE(rArray(iFileSize))
  READ(1,*) rArray
  CLOSE(1)
  ALLOCATE(fGetFileDoublePrecission(iFileSize))
  fGetFileDoublePrecission = rArray
  END FUNCTION fGetFileDoublePrecission
  !----------------------------------------------------------------------------------------------------------------------------
  FUNCTION fGetFileInteger(sFileName,iFileSize)
  IMPLICIT NONE
  INTEGER :: iFileSize
  INTEGER, DIMENSION(:), ALLOCATABLE :: iArray, fGetFileInteger
  CHARACTER(LEN=LONGWORD) :: sFileName

  OPEN(UNIT=1,FILE = sFileName, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
  ALLOCATE(iArray(iFileSize))
  READ(1,*) iArray
  CLOSE(1)
  ALLOCATE(fGetFileInteger(iFileSize))
  fGetFileInteger = iArray
  END FUNCTION fGetFileInteger
  !----------------------------------------------------------------------------------------------------------------------------
  FUNCTION fGetFileString(sFileName,iFileSize)
  IMPLICIT NONE
  INTEGER :: iFileSize
  CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sArray, fGetFileString
  CHARACTER(LEN=LONGWORD) :: sFileName
  
  OPEN(UNIT=1,FILE = sFileName, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
  ALLOCATE(sArray(iFileSize))
  READ(1,*) sArray
  CLOSE(1)
  ALLOCATE(fGetFileString(iFileSize))
  fGetFileString = sArray
  ENDFUNCTION fGetFileString
  !----------------------------------------------------------------------------------------------------------------------------
  FUNCTION fGetFileSize(sFileName)
  IMPLICIT NONE
  CHARACTER(LEN=LONGWORD) :: sFileName
  INTEGER :: fGetFileSize,size, reason

  OPEN(UNIT=1,FILE = sFileName, FORM="FORMATTED", STATUS="OLD", ACTION="READ")
  size = 0
  reason = 0
  DO WHILE (reason == 0)
    READ(1,*,IOSTAT=reason)
    IF (reason > 0) THEN
      PRINT*, 'ERROR READING FILE'
      EXIT
    ELSE
      size=size+1
    ENDIF
  ENDDO
  IF (size == 1) THEN
    PRINT*, ' FILE IS EMPTY'
  ENDIF
  CLOSE(1)
  fGetFileSize= size-1
  ENDFUNCTION fGetFileSize
  !-----------------------------------------------------------------------------------------------------------------------------
  ENDMODULE mGetInput