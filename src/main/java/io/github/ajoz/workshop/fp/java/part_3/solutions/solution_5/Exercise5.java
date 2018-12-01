package io.github.ajoz.workshop.fp.java.part_3.solutions.solution_5;


enum Architecture {
    VLIW,
    CISC,
    RISC,
    MISC,
    ZISC,
    EPIC
}

class HardwareInfo {
    Architecture getArchitecture() {
        return Architecture.VLIW;
    }
}

class DeviceInfo {
    HardwareInfo getHardwareInfo() {
        return new HardwareInfo();
    }
}

class DeviceAPI {
    DeviceInfo getDeviceInfo() {
        return new DeviceInfo();
    }
}

public class Exercise5 {

}
