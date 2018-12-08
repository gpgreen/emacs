;;; ede-projects.el

;; Description: project files for ede

(ede-cpp-root-project "AHRS"
                :name "Attitude Heading Reference Firmware"
                :file "~/src/avfirmware/ahrs/globals.h"
                :include-path '("/"
                                "~/src/avfirmware/libs/avr_drivers/drivers"
                                "~/src/avfirmware/libs/avr_drivers/utils"
                                "~/src/avfirmware/libs/avr_drivers/i2cmaster"
                                "~/src/avfirmware/libs/libcanard"
                               )
                :system-include-path '("/usr/lib/avr/include")
                :spp-table '(("F_CPU" . "8000000UL")))

(ede-cpp-root-project "compass"
                :name "Compass Firmware"
                :file "~/src/avfirmware/compass/globals.h"
                :include-path '("/"
                                "~/src/avfirmware/libs/avr_drivers/drivers"
                                "~/src/avfirmware/libs/avr_drivers/utils"
                                "~/src/avfirmware/libs/avr_drivers/i2cmaster"
                                "~/src/avfirmware/libs/libcanard"
                               )
                :system-include-path '("/usr/lib/avr/include")
                :spp-table '(("F_CPU" . "16000000UL")))

;; ede-projects.el ends here
