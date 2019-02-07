


 call read_satcast(nread,npuse,infile,obstype,lunout,twind,sis)
                string='READ_SATCAST'
!   Write collective obs selection information to scratch file.
    if (lread_obs_save .and. mype==0) then
       write(6,*)'READ_OBS:  write collective obs selection info to ',trim(obs_input_common)
       open(lunsave,file=obs_input_common,form='unformatted')
       write(lunsave) ndata,superp,nprof_gps,ditype
       write(lunsave) super_val1
       close(lunsave)
    endif

!   End of routine
    return
end subroutine read_obs


end module read_obsmod
