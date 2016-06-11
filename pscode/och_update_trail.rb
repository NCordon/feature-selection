
def update_trail(trail, factor_evap, extra, mask){
    trail = (trail * !mask +
        ( (1 - factor_evap) * trail + factor_evap * extra) * mask)
}
