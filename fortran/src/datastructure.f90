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
    INTEGER, DIMENSION(1) :: iNOfDimensions
	INTEGER, DIMENSION(3) :: iNOfGridPoints
    INTEGER, DIMENSION(:), ALLOCATABLE :: iNOfPhses
	INTEGER, DIMENSION(:), ALLOCATABLE :: iNOfElements
    DOUBLEPRECISION, DIMENSION(3) :: rDomainSize
	DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rTime
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rMoleFractions
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rPhaseFractions
    DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rChemicalPotentials
	DOUBLEPRECISION, DIMENSION(:), ALLOCATABLE :: rFiniteVolumeCenterCoordinates
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfPhases
	CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfElements
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
    INTEGER :: iNOfPhases
	INTEGER :: iNOfElements
    INTEGER :: iNOfTimeSteps
	INTEGER :: iNOfDimensions
    DOUBLEPRECISION, DIMENSION(3) :: rDomainSize
    INTEGER, DIMENSION(3) :: iNumberOfGridPoints
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfPhases
    CHARACTER(LEN=SHORTWORD), DIMENSION(:), ALLOCATABLE :: sNamesOfElements
    TYPE(tyTimestepData)    , POINTER, DIMENSION(:)     :: tyTimestepsData
  END TYPE tyProcessedData
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE POSTPROCESSORROOT
    CHARACTER(LEN=LONGWORD) :: sInPath
    CHARACTER(LEN=LONGWORD) :: sOutPath
	TYPE(tyUnprocessedData), POINTER :: tyRaw
	TYPE(tyProcessedData)  , POINTER :: tyProcessed
  ENDTYPE POSTPROCESSORROOT
  !-------------------------------------------------------------------------------------------------------------------------------------
  !main_variables_declaration
  !-------------------------------------------------------------------------------------------------------------------------------------
  TYPE(POSTPROCESSORROOT), POINTER :: tyParrent
  !-------------------------------------------------------------------------------------------------------------------------------------
  ENDMODULE mdsPostProcessor



