import targets
import testlib

class RocketSimHart(targets.Hart):
    pass

class RocketSim(targets.Target):
    harts = [RocketSimHart()]
    timeout_sec = 6000
    server_timeout_sec = 60*60
    openocd_config_path = "RocketSim.cfg"

    def create(self):
        print "STARTING A SIMULATION"
        print self.sim_cmd
        return testlib.VcsSim(sim_cmd=self.sim_cmd, debug=False)
