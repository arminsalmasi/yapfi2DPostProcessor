  MODULE mdsPostProcessor
  !-------------------------------------------------------------------------------------------------------------------------------------
  PUBLIC
  !-------------------------------------------------------------------------------------------------------------------------------------
  INTEGER :: iFunctionReturnValue
  INTEGER,PARAMETER::SHORTWORD=40
  INTEGER,PARAMETER::LONGWORD=200
  INTEGER,PARAMETER::HUGEWORD=1000
  LOGICAL :: lErrorFlag
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE tyUnprocessedData
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rFiniteVolumeCenterCoordinates
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rMoleFractions
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rPhaseFractions
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rChemicalPotentials
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rTime
    DOUBLEPRECISION, DIMENSION(3) :: rDomainSize
    INTEGER, DIMENSION(1) :: iNOfDimensions
    INTEGER, DIMENSION(:), ALLOCATABLE :: iNOfElements
    INTEGER, DIMENSION(:), ALLOCATABLE :: iNOfPhses
    INTEGER, DIMENSION(3) :: iNOfGridPoints
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfElements
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfPhases
  ENDTYPE tyUnprocessedData
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE CellData
    INTEGER :: iVertexIndex
    INTEGER, DIMENSION(3) :: iXYZIndexes
    DOUBLEPRECISION, DIMENSION(3) :: iXYZCoordinates
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rPhaseFractions(:)
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rMoleFractions(:)
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rChemicalPotentials(:)
  ENDTYPE CellData
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE tyTimestepData   !
    DOUBLEPRECISION :: rTime
    INTEGER :: iTimeStep
    TYPE(CellData), POINTER, DIMENSION(:,:,:) :: Nodes
  ENDTYPE tyTimestepData
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE tyProcessedData
    INTEGER :: iNOfDimensions
    DOUBLEPRECISION, DIMENSION(3) :: rDomainSize
    INTEGER, DIMENSION(3) :: iNumberOfGridPoints
    INTEGER :: iNOfElements
    INTEGER :: iNOfPhases
    INTEGER :: iNOfTimeSteps
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfPhases
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfElements
    TYPE(tyTimestepData), POINTER, DIMENSION(:) :: tyTimestepsData
  END TYPE tyProcessedData
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE POSTPROCESSORROOT
    TYPE(tyProcessedData), POINTER :: tyProcessed
    TYPE(tyUnprocessedData), POINTER :: tyRaw
    CHARACTER(LEN=LONGWORD) :: sInPath
    CHARACTER(LEN=LONGWORD) :: sOutPath
  ENDTYPE POSTPROCESSORROOT
  !-------------------------------------------------------------------------------------------------------------------------------------
  !main_variables_declaration
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE(POSTPROCESSORROOT), POINTER :: tyParrent
  !-------------------------------------------------------------------------------------------------------------------------------------
  ENDMODULE mdsPostProcessor



