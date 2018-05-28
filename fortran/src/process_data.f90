  MODULE mDoProcess
  USE mdsPostProcessor
  CONTAINS
  !-------------------------------------------------------------------------------------------------------------------------------------
  FUNCTION fDoProcessData(tyParentTmp)

  IMPLICIT NONE
  INTEGER :: fDoProcessData
  INTEGER :: c,e,p,t,cc,cc_interval,vc,nx,ny,nz,i
  TYPE(POSTPROCESSORROOT), POINTER :: tyParentTmp

  tyParentTmp%tyProcessed%iNOfDimensions = tyParentTmp%tyRaw%iNOfDimensions(1)
  tyParentTmp%tyProcessed%rDomainSize = tyParentTmp%tyRaw%rDomainSize
  tyParentTmp%tyProcessed%iNOfElements = tyParentTmp%tyRaw%iNOfElements(1)
  tyParentTmp%tyProcessed%iNOfPhases = tyParentTmp%tyRaw%iNOfPhses(1)
  tyParentTmp%tyProcessed%iNumberOfGridPoints = tyParentTmp%tyRaw%iNOfGridPoints
  tyParentTmp%tyProcessed%iNOfTimeSteps = size(tyParentTmp%tyRaw%rTime)
  cc_interval = tyParentTmp%tyProcessed%iNOfDimensions

  SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
  CASE (1)
    tyParentTmp%tyProcessed%iNumberOfGridPoints(2:3) = 1 !DIMENSIONALITY_ERROR_HANDLER
  CASE (2)
    tyParentTmp%tyProcessed%iNumberOfGridPoints(3) = 1 !DIMENSIONALITY_ERROR_HANDLER
    ! CASE (3) PROCEED
  END SELECT
  iFunctionReturnValue = fAllocatePrecessedData(tyParentTmp)
  tyParentTmp%tyProcessed%sNamesOfPhases = tyParentTmp%tyRaw%sNamesOfPhases
  tyParentTmp%tyProcessed%sNamesOfElements = tyParentTmp%tyRaw%sNamesOfElements
  e = 1 !counter-all-data-elementwise
  p = 1 !counter_all_data_pahseswise
  DO t = 1,tyParentTmp%tyProcessed%iNOfTimeSteps
    tyParentTmp%tyProcessed%tyTimestepsData(t)%iTimeStep = t
    tyParentTmp%tyProcessed%tyTimestepsData(t)%rTime = tyParentTmp%tyRaw%rTime(t)
    cc = 1 !counter for coordinates !initializing
    vc = 0 !counter on vertex indexes
    DO nx=1, tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
      DO ny=1, tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
        DO nz=1, tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
          tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iVertexIndex = vc
          tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iXYZIndexes(:) = (/nx,ny,nz/)
          SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
          CASE (1)
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iXYZCoordinates(:) = (/tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc), 0.0D0, 0.0D0/)!adjust_number_of_coordinates_with_dimensio
          CASE (2)
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iXYZCoordinates(:) = (/tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc), tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc+1), 0.0D0/)!adjust_number_of_coordinates_with_dimensio
          CASE (3)
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iXYZCoordinates(:) = (/tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc), tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc+1), tyParentTmp%tyRaw%rFiniteVolumeCenterCoordinates(cc+2)/)!adjust_number_of_coordinates_with_dimensio
          END SELECT
          cc= cc+cc_interval !adjust_number_of_coordinates_with_DIMENSIONality
          !prob_elements
          DO i = 1, tyParentTmp%tyProcessed%iNOfElements
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rMoleFractions(i) = tyParentTmp%tyRaw%rMoleFractions(e)
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rChemicalPotentials(i) = tyParentTmp%tyRaw%rChemicalPotentials(e)
            e=e+1
          ENDDO
          !prob_phases
          DO i = 1, tyParentTmp%tyProcessed%iNOfPhases
            tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rPhaseFractions(i) = tyParentTmp%tyRaw%rPhaseFractions(p)
            p=p+1
          ENDDO
          vc= vc+1
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  fDoProcessData = 0
  ENDFUNCTION fDoProcessData
  !-------------------------------------------------------------------------------------------------------------------------------------
  FUNCTION fAllocatePrecessedData(tmp)
  IMPLICIT NONE
  INTEGER :: fAllocatePrecessedData, t,nx,ny,nz
  TYPE(POSTPROCESSORROOT), POINTER ::tmp

  ALLOCATE(tmp%tyProcessed%tyTimestepsData(tmp%tyProcessed%iNOfTimeSteps))
  DO t = 1,tmp%tyProcessed%iNOfTimeSteps
    ALLOCATE(tmp%tyProcessed%tyTimestepsData(t)%Nodes(tmp%tyProcessed%iNumberOfGridPoints(1),tmp%tyProcessed%iNumberOfGridPoints(2),tmp%tyProcessed%iNumberOfGridPoints(3)))
    DO nx=1, tmp%tyProcessed%iNumberOfGridPoints(1)
      DO ny=1, tmp%tyProcessed%iNumberOfGridPoints(2)
        DO nz=1, tmp%tyProcessed%iNumberOfGridPoints(3)
          ALLOCATE(tmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rMoleFractions(tmp%tyProcessed%iNOfElements))
          ALLOCATE(tmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rChemicalPotentials(tmp%tyProcessed%iNOfElements))
          ALLOCATE(tmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rPhaseFractions(tmp%tyProcessed%iNOfPhases))
        ENDDO
      ENDDO
    ENDDO
  ENDDO
  fAllocatePrecessedData = 0
  ENDFUNCTION fAllocatePrecessedData
  !-------------------------------------------------------------------------------------------------------------------------------------
  END MODULE mDoProcess