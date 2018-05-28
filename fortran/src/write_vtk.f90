  MODULE mSetVtk
  USE mdsPostProcessor
  CONTAINS
  !-------------------------------------------------------------------------------------------------------------------------------------
  FUNCTION fSetVtkFile(tyParentTmp)
  IMPLICIT NONE
  INTEGER :: fSetVtkFile
  TYPE(POSTPROCESSORROOT), POINTER :: tyParentTmp
  INTEGER :: e,p,t,nx,ny,nz,CNTX,CNTY,CNTZ, iNOfPoints,iNOfCells, iSizeOfCellList
  CHARACTER(LEN=LONGWORD) :: sFileName,sFileExtension,sFileNameHeader,sElementName, sTimeStepNumeber
  CHARACTER(LEN=LONGWORD) :: sElementNumber, sFilePath, sTempStr,sPhaseNumber,sPhaseName

  DO t = 1,tyParentTmp%tyProcessed%iNOfTimeSteps
    !fiLEName
    sFileName = ''
    sFileNameHeader = 'VTK_timestep_'
    sFileExtension='.vtk'
    WRITE (sTimeStepNumeber, *) t
    sFilePath = tyParentTmp%sOutPath
    sFileName = TRIM(ADJUSTL(sFilePath))//TRIM(ADJUSTL(sFileNameHeader))//TRIM(ADJUSTL(sTimeStepNumeber))//TRIM(ADJUSTL(sFileExtension))
    OPEN(UNIT=1,FILE = sFileName, FORM="FORMATTED",  ACTION="WRITE")
    !fileheader
    WRITE(1,'(a)')'# vtk DataFile Version 2.0'
    WRITE(1,'(a)') 'All information at time step: '//TRIM(ADJUSTL(sTimeStepNumeber))//' \n'
    WRITE(1,'(a)')'ASCII'
    WRITE(1,'(a)')'DATASET UNSTRUCTURED_GRID'
    !coordintes
    sTempStr=''
    iNOfPoints = tyParentTmp%tyProcessed%iNumberOfGridPoints(1)*tyParentTmp%tyProcessed%iNumberOfGridPoints(2) * tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
    WRITE(sTempStr,*) iNOfPoints
    sTempStr = 'POINTS'//' '//(TRIM(ADJUSTL(sTempStr)))//' '//'Double'
    WRITE(1,'(a)') sTempStr
    DO nx = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
      DO ny = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
        DO nz = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
          WRITE(1,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iXYZCoordinates(:)
        ENDDO
      ENDDO
    ENDDO
    !number of cells
    CNTX = tyParentTmp%tyProcessed%iNumberOfGridPoints(1)-1
    CNTY = tyParentTmp%tyProcessed%iNumberOfGridPoints(2)-1
    CNTZ = tyParentTmp%tyProcessed%iNumberOfGridPoints(3)-1
    SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
    CASE (1)
      iNOfCells = (tyParentTmp%tyProcessed%iNumberOfGridPoints(1)-1)
      CNTY = 1
      CNTZ = 1
    CASE (2)
      iNOfCells = (tyParentTmp%tyProcessed%iNumberOfGridPoints(1)-1)*(tyParentTmp%tyProcessed%iNumberOfGridPoints(2)-1)
      CNTZ = 1
    CASE (3)
      iNOfCells = (tyParentTmp%tyProcessed%iNumberOfGridPoints(1)-1)*(tyParentTmp%tyProcessed%iNumberOfGridPoints(2)-1)*(tyParentTmp%tyProcessed%iNumberOfGridPoints(3)-1) !*#
      !PROCEED
    END SELECT
    iSizeOfCellList = iNOfCells * ((2**tyParentTmp%tyProcessed%iNOfDimensions)+1)
    sTempStr=''
    WRITE(sTempStr,*) iNOfCells
    WRITE(1,'(a)',ADVANCE="NO") 'CELLS '// TRIM(ADJUSTL(sTempStr))//' '
    sTempStr=''
    WRITE(sTempStr,*) iSizeOfCellList
    WRITE(1,'(a)') TRIM(ADJUSTL(sTempStr))
    !vertex index
    DO nx = 1,CNTX
      DO ny = 1 ,CNTY
        DO nZ = 1 ,CNTZ
          SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
          CASE(1)
            sTempStr =''
            WRITE(sTempStr,*) 2**tyParentTmp%tyProcessed%iNOfDimensions
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny,nz)%iVertexIndex
            WRITE(1,'(a)') TRIM(ADJUSTL(sTempStr))//' '
          CASE(2)
            sTempStr =''
            WRITE(sTempStr,*) 2**tyParentTmp%tyProcessed%iNOfDimensions
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny+1,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny+1,nz)%iVertexIndex
            WRITE(1,'(a)') TRIM(ADJUSTL(sTempStr))
          CASE(3)
            sTempStr =''
            WRITE(sTempStr,*) 2**tyParentTmp%tyProcessed%iNOfDimensions
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz+1)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny+1,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny+1,nz+1)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny,nz+1)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny+1,nz)%iVertexIndex
            WRITE(1,'(a)',ADVANCE="NO") TRIM(ADJUSTL(sTempStr))//' '
            sTempStr =''
            WRITE(sTempStr,*) tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx+1,ny+1,nz+1)%iVertexIndex
            WRITE(1,'(a)') TRIM(ADJUSTL(sTempStr))
          END SELECT
        ENDDO
      ENDDO
    ENDDO
    !cellTYPE
    sTempStr =''
    WRITE(sTempStr,*) iNOfCells
    WRITE(1,'(a)') 'CELL_TYPES'//' '//TRIM(ADJUSTL(sTempStr))
    SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
    CASE (1)
      DO nx= 1,iNOfCells
        WRITE(1,'(a)',ADVANCE="NO") '4 '
      ENDDO
    CASE(2)
      DO nx= 1,iNOfCells
        WRITE(1,'(a)',ADVANCE="NO") '8 '
      ENDDO
    CASE(3)
      DO nx= 1,iNOfCells
        WRITE(1,'(a)',ADVANCE="NO") '11 '
      ENDDO
    END SELECT
    !number of points
    WRITE(1,*)
    sTempStr =''
    WRITE(sTempStr,*) iNOfPoints
    WRITE(1,'(a)') 'POINT_DATA'//' '//TRIM(ADJUSTL(sTempStr))
    !WRITE scalers of chemical potential and mole fractions
    DO e = 1,tyParentTmp%tyProcessed%iNOfElements
      !header line mole fractions
      WRITE (sElementNumber, *) e
      sElementName = tyParentTmp%tyProcessed%sNamesOfElements(e)
      WRITE(1,'(a)') 'SCALARS X('//TRIM(ADJUSTL(sElementName))//')'//' '//'Double 1'
      WRITE(1,'(a)') 'LOOKUP_TABLE default'

      CNTX = tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
      CNTY = tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
      CNTZ = tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
      SELECT CASE (tyParentTmp%tyProcessed%iNOfDimensions)
      CASE (1)
        CNTY = 1
        CNTZ = 1
      CASE (2)
        CNTZ = 1
      CASE (3)
        ! PROCEED
      END SELECT
      !sclers mole fractions
      DO nx = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
        DO ny = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
          DO nZ = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
            WRITE(1,'(ES28.16)',ADVANCE="NO") tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rMoleFractions(e)
          ENDDO
        ENDDO
      ENDDO
      WRITE(1,'(a)')
      !header line chemical potentials
      WRITE(1,'(a)')'SCALARS CHP('//TRIM(ADJUSTL(sElementName))//')'//' '//'Double 1'
      WRITE(1,'(a)')'LOOKUP_TABLE default'
      ! scalers chemical potentials
      DO nx = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
        DO ny = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
          DO nZ = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
            WRITE(1,'(ES28.16)',ADVANCE="NO") tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rChemicalPotentials(e)
          ENDDO
        ENDDO
      ENDDO
      WRITE(1,'(a)')
    ENDDO
    !scalers of phase fractions
    DO p = 1,tyParentTmp%tyProcessed%iNOfPhases
      !header line pahse fraction
      WRITE (sPhaseNumber, *) p
      sPhaseName = tyParentTmp%tyProcessed%sNamesOfPhases(p)
      WRITE(1,'(a)')'SCALARS PHF('//TRIM(ADJUSTL(sPhaseName))//')'//' '//'Double 1'
      WRITE(1,'(a)')'LOOKUP_TABLE default'
      !scalers phase fractions
      DO nx = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(1)
        DO ny = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(2)
          DO nZ = 1,tyParentTmp%tyProcessed%iNumberOfGridPoints(3)
            WRITE(1,'(ES28.16)',ADVANCE="NO") tyParentTmp%tyProcessed%tyTimestepsData(t)%Nodes(nx,ny,nz)%rPhaseFractions(p)
          ENDDO
        ENDDO
      ENDDO
      WRITE(1,'(a)')
    ENDDO
    ! CLOSE FILE
    CLOSE(1)
  ENDDO ! next timestep
  fSetVtkFile = 0
  ENDFUNCTION fSetVtkFile
  !-------------------------------------------------------------------------------------------------------------------------------------
  END MODULE mSetVtk