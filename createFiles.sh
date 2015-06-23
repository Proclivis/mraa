# c2hs --cppopts='-I/usr/include' --output='src/System/Hardware/I2C/Linux/I2CDev.hs' 'src/System/Hardware/I2C/Linux/chs/I2CDev.chs'
c2hs --cppopts='-I/usr/include' --output='src/System/Hardware/Mraa/Mraa.hs' 'src/System/Hardware/Mraa/chs/Mraa.chs'
# c2hs --cppopts='-Isrc/cbits' --include='src/cbits' --output='src/System/Hardware/I2C/Linux/SMBusDev2.hs' 'src/System/Hardware/I2C/Linux/chs/SMBusDev2.chs'
# rm src/System/Hardware/I2C/Linux/*.h
# rm src/System/Hardware/I2C/Linux/*.chi
rm src/System/Hardware/Mraa/*.h
rm src/System/Hardware/Mraa/*.chi