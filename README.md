# lickometer-library
R library for cpl-lab lickometer

# Headers definition

| variable_name | definition |
| --- | --- |
| source | full path of the original file |
| sensor | number of sensor (left/right spout) |
| tiempo | ms from the start of lickometer software |
| actividad | number of licks (cumulative) |
| evento | number of events (cumulative) |
| exito | number of rewarded events (cumulative) |
| fecha | experiment date |
| fecha_ms | experiment date is posix epoch |
| tiempo_fecha_ms | experiment date including HMS in posix epoch |
| tiempo_fecha | experimento date including HMS
| n_sesion | session number |
| droga | orexin, dynorphin, nor-bni, etc. |
| dosis | TBD |
| hora inicio/fin | exact time when experiment started and ended |
| n_licometro | lickometer hardware number |
| estimulo spout 1/2 | reward delivered i.e. water, sucrose, etc. |
| licks inicio/fin | researcher notes on number of licks at the start of end of experiment |
| eventos inicio/fin | sames as above but for events |
| ml_consumidos | syringe ml levels |


# TODO

1. Add metadata columns (canulated yes/no, histological information)



