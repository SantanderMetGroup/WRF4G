

cdo genbil,/vols/tetis/escena/CORDEX/01_CDX_Sensitivity/data/obs/CRUTS21AFR_sftlf.nc CORDEX_UC_WRF_SEN2CTRL_sftlf.nc bilweights
cdo remap,/vols/tetis/escena/CORDEX/01_CDX_Sensitivity/data/obs/CRUTS21AFR_sftlf.nc,bilweights CORDEX_UC_WRF_SEN2CTRL_sftlf.nc pp
cdo setrtoc,0,0.99999,0 pp CRU_mask.nc
