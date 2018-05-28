  PROGRAM YAPFIPost

  USE mdsPostProcessor, 
  USE mGetInput
  USE mDoProcess
  USE mSetVtk

  IMPLICIT NONE

  ALLOCATE(tyParrent)
  ALLOCATE(tyParrent%tyRaw)
  ALLOCATE(tyParrent%tyProcessed)

  WRITE(*,*) "Inpout directory path (Put '\' at the end):"
  READ(*,*) tyParrent%sInPath
  WRITE(*,*) "Outout directory path  (Put '\' at the end):"
  READ(*,*) tyParrent%sOutPath
  !tyParrent%sInPath ='C:\USErs\salmasi\DOcuments\Mycodes\yapfiPP\test1D\'         !temporary
  !tyParrent%sOutPath ='C:\USErs\salmasi\DOcuments\Mycodes\yapfiPP\test1D\vtk\'    !temporary
  WRITE(*,*) '** Reading the data'
  iFunctionReturnValue = fGetRawData(tyParrent)
  WRITE(*,*) '** Processing the data'
  iFunctionReturnValue = fDoProcessData(tyParrent)
  WRITE(*,*) '** writing to output files'
  iFunctionReturnValue = fSetVtkFile(tyParrent)
  WRITE(*,*) 'End of process, press return!'
  READ(*,*)
  DEALLOCATE(tyParrent)

  ENDPROGRAM YAPFIPost