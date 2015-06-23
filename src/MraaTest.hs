-----------------------------------------------------------------------------
--
-- Module      :  MraaTest
-- Copyright   :  Michael Jones
-- License     :  BSD3
--
-- Maintainer  :  Michael Jones
-- Stability   :  Experimental
-- Portability :  Edison + BELinux
--
-- | Test mraa interrace.
--
-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}

module Main (
    main
) where

import System.Hardware.Serialport
import Control.Concurrent
import Numeric
import System.Hardware.Mraa.Mraa

runMraaTest :: IO ()
runMraaTest  = do

    putStrLn "Init"
    m <- mraaInit
    print m

    putStr "Platform: "
    name <- mraaGetPlatformName
    print name

    putStr "Platform Type: "
    t <- mraaGetPlatformType
    print t

    putStr "Platform Version: "
    v <- mraaGetVersion
    print v


    putStrLn "Open I2C"
    i2c <- mraaI2cInit 1
    print i2c

    putStr "Byte 0: "
    m <- mraaI2cAddress i2c 0x50
    d <- mraaI2cReadByteData i2c 0x00
--    m <- mraaI2cStop i2c
    print $ showHex d ""

    putStr "Byte 1: "
    d <- mraaI2cReadByteData i2c 0x01
    print $ showHex d ""

--    m <- mraaI2cStop i2c
    putStr "Byte 2: "
    d <- mraaI2cReadByteData i2c 0x02
    print $ showHex d ""
--    m <- mraaI2cStop i2c

    putStrLn "Open SPI"
    spi <- mraaSpiInitRaw 5 1
    print spi

    r <- mraaSpiStop spi
    return ()

    putStrLn "Pre Open UART"
    sp <- openSerial "/dev/ttyMFD1" defaultSerialSettings { commSpeed = CS115200 }
    closeSerial sp

    putStrLn "Open UART"
    uart <- mraaUartInit 0
    print uart

    putStr "Read Signature: "
    r <- mraaUartSetBaudrate uart 115200
    r <- mraaUartSetMode uart 8 MraaUartParityNone 1
    r <- mraaUartSetFlowcontrol uart False False
    r <- mraaUartWrite uart [0x81] 1
    (r,sig) <- mraaUartRead uart 6
    print sig

    putStrLn "Open GPIO"
    gpio <- mraaGpioInit 23
    print gpio

    pin <- mraaGpioGetPin gpio
    print pin

    r <- mraaGpioEdgeMode gpio MraaGpioEdgeNone
    r <- mraaGpioMode gpio MraaGpioStrong
    r <- mraaGpioDir gpio MraaGpioOut
    r <- mraaGpioWrite gpio 1 -- CS0 pin
    r <- mraaGpioClose gpio

    putStrLn "Open SPI"
    gpio <- mraaSpiInit 0
    print spi

    r <- mraaSpiMode spi MraaSpiMode0
    r <- mraaSpiFrequency spi 1000000
    r <- mraaSpiBitPerWord spi 8

    putStrLn "Status: "
    (r,w) <- mraaSpiTransferBuf spi [0x90, 0x55] 2 -- 10 001 000, 0x55, 
    (r,w) <- mraaSpiTransferBuf spi [0x50, 0x00] 2 --                 , 01 001 000, 0x00
    print $ showHex (w!!1) ""
    (r,w) <- mraaSpiTransferBuf spi [0x90, 0xAA] 2 -- 10 001 000, 0xAA, 
    (r,w) <- mraaSpiTransferBuf spi [0x50, 0x00] 2 --                 , 01 001 000, 0x00
    print $ showHex (w!!1) ""
    r <- mraaSpiWrite spi 0x30 -- 00 110 000
    (r,w) <- mraaSpiTransferBuf spi [0x70, 0x00] 2 -- 01 110 000 
    print $ showHex (w!!1) ""

    return ()

exeMain :: IO ()
exeMain = do
    runMraaTest

#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION

