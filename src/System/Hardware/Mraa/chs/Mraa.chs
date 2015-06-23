{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
#include <stdint.h>
#include <mraa.h>
#include <mraa/types.h>
#include <mraa/common.h>
#include <mraa/i2c.h>
#include <mraa/uart.h>
#include <mraa/spi.h>
#include <mraa/gpio.h>

{# context  lib="libmraa" prefix="mraa"#}

module System.Hardware.Mraa.Mraa (
  MRAA_RESULT(..),

  mraaInit,
  mraaDeinit,
  mraaPinModeTest,
  mraaAdcRawBits,
  mraaAdcSupportedBits,
  mraaSetLogLevel,
  mraaGetPlatformName,
  mraaSetPriority,
  mraaGetVersion,
  mraaResultPrint,
  mraaGetPlatformType,
  mraaGetPinCount,
  mraaGetPinName,

  mraaI2cInit,
  mraaI2cInitRaw,
  mraaI2cFrequency,
  mraaI2CRead,
  mraaI2cReadByte,
  mraaI2cReadByteData,
  mraaI2cReadWordData,
  mraaI2CReadBytesData,
  mraaI2cWrite,
  mraaI2cWriteByte,
  mraaI2cWriteByteData,
  mraaI2cWriteWordData,
  mraaI2cAddress,
  mraaI2cStop,

  mraaSpiInit,
  mraaSpiInitRaw,
  mraaSpiMode, MRAA_SPI_MODE(..),
  mraaSpiFrequency,
  mraaSpiWrite,
  mraaSpiWriteWord,
  mraaSpiWriteBuf,
  mraaSpiWriteBufWord,
  mraaSpiTransferBuf,
  mraaSpiTransferBufWord,
  mraaSpiLsbmode,
  mraaSpiBitPerWord,
  mraaSpiStop,

  mraaGpioInit,
  mraaGpioInitRaw,
  mraaGpioEdgeMode, MRAA_GPIO_EDGE(..),
  mraaGpioMode, MRAA_GPIO_MODE(..),
  mraaGpioDir, MRAA_GPIO_DIR(..),
  mraaGpioClose,
  mraaGpioRead,
  mraaGpioWrite,
  mraaGpioOwner,
  mraaGpioUseMmapped,
  mraaGpioGetPin,
  mraaGpioGetPinRaw,

  UARTHandlePtr,
  mraaUartInit,
  mraaUartInitRaw,
  mraaUartFlush,
  mraaUartSetBaudrate,
  mraaUartSetMode, MRAA_UART_PARITY(..),
  mraaUartSetFlowcontrol,
  mraaUartSetTimeout,
  mraaUartGetDevPath,
  mraaUartStop,
  mraaUartRead,
  mraaUartWrite,
  mraaUartDataAvailable,

  ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C
import Foreign.Marshal
import Foreign.Storable
import System.IO.Unsafe
--import Data.Functor
--import Control.Applicative
import Control.Monad
import Data.Word
--import System.Posix.IOCtl as I

--type U08 = {#type uint8_t#}
--type U16 = {#type uint16_t#}
--type U32 = {#type uint32_t#}
--type U64 = {#type uint64_t#}
--type S08 = {#type int8_t#}
--type S16 = {#type int16_t#}
--type S32 = {#type int32_t#}
--type S64 = {#type int64_t#}
--type C08 = {#type __u8#}

allocateU32 :: (Ptr CUChar -> IO b) -> IO b
allocateU32 = allocaArray 32
peekU32 :: Ptr CUChar -> IO [CUChar]
peekU32 = peekArray 32

allocate32 :: (Ptr CChar -> IO b) -> IO b
allocate32 = allocaArray 32
peek32 :: Ptr CChar -> IO [CChar]
peek32 = peekArray 32

allocateU32s :: (Ptr CUShort -> IO b) -> IO b
allocateU32s = allocaArray 32
peekU32s :: Ptr CUShort -> IO [CUShort]
peekU32s = peekArray 32

-- This one uses the array, so the unsafe call better not delete it
withUCharArray :: (Storable a, Integral a) => [a] -> (Ptr CUChar -> IO b) -> IO b
withUCharArray = withArray . liftM fromIntegral
withCharArray :: (Storable a, Integral a) => [a] -> (Ptr CChar -> IO b) -> IO b
withCharArray = withArray . liftM fromIntegral

-- This one uses the array, so the unsafe call better not delete it
withUShortArray :: (Storable a, Integral a) => [a] -> (Ptr CUShort -> IO b) -> IO b
withUShortArray = withArray . liftM fromIntegral

{#enum define MRAA_PLATFORM {
    MRAA_INTEL_GALILEO_GEN1 as MraaIntelGalileoGen1,
    MRAA_INTEL_GALILEO_GEN2 as MraaIntelGalileoGen2,
    MRAA_INTEL_EDISON_FAB_C as MraaIntelEdisonFabC,
    MRAA_INTEL_DE3815 as MraaIntelDe3815,
    MRAA_INTEL_MINNOWBOARD_MAX as MraaIntelMinnoboardMax,
    MRAA_RASPBERRY_PI as MraaRasberryPi,
    MRAA_BEAGLEBONE as MraaBeaglebone,
    MRAA_BANANA as MraaBananna,
    MRAA_UNKNOWN_PLATFORM as MraaUnknownPlatform}
deriving (Eq,Ord,Show) #}

{#enum define MRAA_RESULT {
    MRAA_SUCCESS as MraaSuccess,
    MRAA_ERROR_FEATURE_NOT_IMPLEMENTED as MraaErrorFeatureNotImplemented,
    MRAA_ERROR_FEATURE_NOT_SUPPORTED as MraaErrorFeatureNotSupported,
    MRAA_ERROR_INVALID_VERBOSITY_LEVEL as MraaErrorInvalidVerbosityLevel,
    MRAA_ERROR_INVALID_PARAMETER as MraaErrorInvalidParameter,
    MRAA_ERROR_INVALID_HANDLE as MraaErrorInvalidHandle,
    MRAA_ERROR_NO_RESOURCES as MraaErrorNoResources,
    MRAA_ERROR_INVALID_RESOURCE as MraaErrorInvalidResource,
    MRAA_ERROR_INVALID_QUEUE_TYPE as MraaErrorInvalidQueueType,
    MRAA_ERROR_NO_DATA_AVAILABLE as MraaErrorNoDataAvailable,
    MRAA_ERROR_INVALID_PLATFORM as MraaErrorInvalidPlatform,
    MRAA_ERROR_PLATFORM_NOT_INITIALISED as MraaErrorPlatformNotInitialized,
    MRAA_ERROR_PLATFORM_ALREADY_INITIALISED as MraaErrorPlatformAlreadyInitialized,
    MRAA_ERROR_UNSPECIFIED as MraaErrorUnspecified}
deriving (Eq,Ord,Show) #}

{#enum define MRAA_PINMODES {
    MRAA_PIN_VALID as MraaPinValid,
    MRAA_PIN_GPIO as MraaPinGpio,
    MRAA_PIN_PWM as MraaPinPwm,
    MRAA_PIN_FAST_GPIO as MraaPinFastGpio,
    MRAA_PIN_SPI as MraaPinSpi,
    MRAA_PIN_I2C as MraaPinI2c,
    MRAA_PIN_AIO as MraaPinAio,
    MRAA_PIN_UART as MraaPinUart}
deriving (Eq,Ord,Show) #}

{#enum define MRAA_I2C_MODE {
    MRAA_I2C_STD as MraaI2cStd,
    MRAA_I2C_FAST as MraaI2cFast,
    MRAA_I2C_HIGH as MraaI2cHigh}
deriving (Eq,Ord,Show) #}

{#enum define MRAA_UART_PARITY {
  MRAA_UART_PARITY_NONE as MraaUartParityNone,
  MRAA_UART_PARITY_EVEN as MraaUartParityEven,
  MRAA_UART_PARITY_ODD as MraaUartParityOdd,
  MRAA_UART_PARITY_MARK as MraaUartParityMark,
  MRAA_UART_PARITY_SPACE as MraaUartParitySpace}
deriving (Eq,Ord,Show) #}

{#fun unsafe mraa_init as mraaInit
  {} -> `MRAA_RESULT' #}

{#fun unsafe mraa_deinit as mraaDeinit
  {} -> `()' #}

-- Need bool
{#fun unsafe mraa_pin_mode_test as mraaPinModeTest
  {`CInt', `MRAA_PINMODES' } -> `CUInt' #}

{#fun unsafe mraa_adc_raw_bits as mraaAdcRawBits
  {} -> `CUInt' #}

{#fun unsafe mraa_adc_supported_bits as mraaAdcSupportedBits
 {} -> `CUInt' #}

{#fun unsafe mraa_set_log_level as mraaSetLogLevel
  {`CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_get_platform_name as mraaGetPlatformName
  {} -> `String' #}

{#fun unsafe mraa_set_priority as mraaSetPriority
  {`CUInt'} -> `CInt' #}

{#fun unsafe mraa_get_version as mraaGetVersion
 {} -> `String' #}

{#fun unsafe mraa_result_print as mraaResultPrint
  {`MRAA_RESULT'} -> `()' #}

toMraaPlatform = toEnum . fromIntegral

{#fun unsafe mraa_get_platform_type as mraaGetPlatformType
  {} -> `MRAA_PLATFORM' toMraaPlatform #}

{#fun unsafe mraa_get_pin_count as mraaGetPinCount
  {} -> `CUInt' #}

{#fun unsafe mraa_get_pin_name as mraaGetPinName
  {`CInt'} -> `String' #}

data I2CHandle
{#pointer *_i2c as I2CHandlePtr -> I2CHandle #}

{#fun unsafe mraa_i2c_init as mraaI2cInit
  {`CInt'} -> `I2CHandlePtr' #}

{#fun unsafe mraa_i2c_init_raw as mraaI2cInitRaw
 {`CUInt'} -> `I2CHandlePtr' #}

{#fun unsafe mraa_i2c_frequency as mraaI2cFrequency
  {`I2CHandlePtr', `MRAA_I2C_MODE' } -> `MRAA_RESULT' #}

{#fun unsafe mraa_i2c_read as mraaI2CRead
  {`I2CHandlePtr', allocateU32- `[CUChar]' peekU32*, `CInt'} -> `CInt' #}

{#fun unsafe mraa_i2c_read_byte as mraaI2cReadByte
 {`I2CHandlePtr'} -> `Word8' #}

{#fun unsafe mraa_i2c_read_byte_data as mraaI2cReadByteData
  {`I2CHandlePtr', `Word8'} -> `Word8' #}

{#fun unsafe mraa_i2c_read_word_data as mraaI2cReadWordData
  {`I2CHandlePtr', `Word8'} -> `Word16' #}

{#fun unsafe mraa_i2c_read_bytes_data as mraaI2CReadBytesData
  {`I2CHandlePtr', `Word8', allocateU32- `[CUChar]' peekU32*, `CInt'} -> `CInt' #}

{#fun unsafe mraa_i2c_write as mraaI2cWrite
  {`I2CHandlePtr', withUCharArray* `[CUChar]', `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_i2c_write_byte as mraaI2cWriteByte
  {`I2CHandlePtr', `Word8'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_i2c_write_byte_data as mraaI2cWriteByteData
  {`I2CHandlePtr', `Word8', `Word8'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_i2c_write_word_data as mraaI2cWriteWordData
  {`I2CHandlePtr', `Word16', `Word8'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_i2c_address as mraaI2cAddress
  {`I2CHandlePtr', `Word8'} -> `()' #}

{#fun unsafe mraa_i2c_stop as mraaI2cStop
  {`I2CHandlePtr'} -> `MRAA_RESULT' #}

{#enum define MRAA_SPI_MODE {
    MRAA_SPI_MODE0 as MraaSpiMode0,
    MRAA_SPI_MODE1 as MraaSpiMode1,
    MRAA_SPI_MODE2 as MraaSpiMode2,
    MRAA_SPI_MODE3 as MraaSpiMode3}
deriving (Eq,Ord,Show) #}

data SPIHandle
{#pointer *_spi as SPIHandlePtr -> SPIHandle #}

{#fun unsafe mraa_spi_init as mraaSpiInit
  {`CInt'} -> `SPIHandlePtr' #}

{#fun unsafe mraa_spi_init_raw as mraaSpiInitRaw
 {`CUInt', `CUInt'} -> `SPIHandlePtr' #}

{#fun unsafe mraa_spi_mode as mraaSpiMode
  {`SPIHandlePtr', `MRAA_SPI_MODE'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_frequency as mraaSpiFrequency
  {`SPIHandlePtr', `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_write as mraaSpiWrite
  {`SPIHandlePtr', `Word8'} -> `CInt' #}

{#fun unsafe mraa_spi_write_word as mraaSpiWriteWord
  {`SPIHandlePtr', `Word16'} -> `CInt' #}

-- Not freeing returned array
{#fun unsafe mraa_spi_write_buf as mraaSpiWriteBuf
  {`SPIHandlePtr', withUCharArray* `[CUChar]', `CInt'} -> `[CUChar]' peekU32* #}

-- Not freeing returned array
{#fun unsafe mraa_spi_write_buf_word as mraaSpiWriteBufWord
  {`SPIHandlePtr', withUShortArray* `[CUShort]', `CInt'} -> `[CUShort]' peekU32s* #}

{#fun unsafe mraa_spi_transfer_buf as mraaSpiTransferBuf
  {`SPIHandlePtr', withUCharArray* `[CUChar]', allocateU32- `[CUChar]' peekU32*, `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_transfer_buf_word as mraaSpiTransferBufWord
  {`SPIHandlePtr', withUShortArray* `[CUShort]', allocateU32s- `[CUShort]' peekU32s*, `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_lsbmode as mraaSpiLsbmode
  {`SPIHandlePtr', `Bool'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_bit_per_word as mraaSpiBitPerWord 
  {`SPIHandlePtr', `CUInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_spi_stop as mraaSpiStop
  {`SPIHandlePtr'} -> `MRAA_RESULT' #}

{#enum define MRAA_GPIO_MODE {
    MRAA_GPIO_STRONG as MraaGpioStrong,
    MRAA_GPIO_PULLUP as MraaGpioPullup,
    MRAA_GPIO_PULLDOWN as MraaGpioPulldown,
    MRAA_GPIO_HIZ as MraaGpioHiz}
deriving (Eq,Ord,Show) #}

{#enum define MRAA_GPIO_DIR {
    MRAA_GPIO_OUT as MraaGpioOut,
    MRAA_GPIO_IN as MraaGpioIn,
    MRAA_GPIO_OUT_HIGH as MraaGpioOutHigh,
    MRAA_GPIO_OUT_LOW as MraaGpioOutLow}
deriving (Eq,Ord,Show) #}


{#enum define MRAA_GPIO_EDGE {
    MRAA_GPIO_EDGE_NONE as MraaGpioEdgeNone,
    MRAA_GPIO_EDGE_BOTH as MraaGpioEdgeBoth,
    MRAA_GPIO_EDGE_RISING as MraaGpioEdgeRising,
    MRAA_GPIO_EDGE_FALLING as MraaGpioEdgeFalling}
deriving (Eq,Ord,Show) #}

data GPIOHandle
{#pointer *_gpio as GPIOHandlePtr -> GPIOHandle #}

{#fun unsafe mraa_gpio_init as mraaGpioInit
  {`CInt'} -> `GPIOHandlePtr' #}

{#fun unsafe mraa_gpio_init_raw as mraaGpioInitRaw
 {`CInt'} -> `GPIOHandlePtr' #}

{#fun unsafe mraa_gpio_edge_mode as mraaGpioEdgeMode
  {`GPIOHandlePtr', `MRAA_GPIO_EDGE'} -> `MRAA_RESULT' #}

{-
 mraa_result_t mraa_gpio_isr(mraa_gpio_context dev, gpio_edge_t edge, void (*fptr)(void*), void* args);
 mraa_result_t mraa_gpio_isr_exit(mraa_gpio_context dev);
-}

{#fun unsafe mraa_gpio_mode as mraaGpioMode
  {`GPIOHandlePtr', `MRAA_GPIO_MODE'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_dir as mraaGpioDir
  {`GPIOHandlePtr', `MRAA_GPIO_DIR'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_close as mraaGpioClose
  {`GPIOHandlePtr'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_read as mraaGpioRead
  {`GPIOHandlePtr'} -> `CInt' #}

{#fun unsafe mraa_gpio_write as mraaGpioWrite
  {`GPIOHandlePtr', `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_owner as mraaGpioOwner
  {`GPIOHandlePtr', `Bool'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_use_mmaped as mraaGpioUseMmapped
  {`GPIOHandlePtr', `Bool'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_gpio_get_pin as mraaGpioGetPin
  {`GPIOHandlePtr'} -> `CInt' #}

{#fun unsafe mraa_gpio_get_pin_raw as mraaGpioGetPinRaw
  {`GPIOHandlePtr'} -> `CInt' #}

data UARTHandle
{#pointer *_uart as UARTHandlePtr -> UARTHandle #}

{#fun unsafe mraa_uart_init as mraaUartInit
  {`Int'} -> `UARTHandlePtr' #}

{#fun unsafe mraa_uart_init_raw as mraaUartInitRaw
 {withCharArray* `[CChar]'} -> `UARTHandlePtr' #}

{#fun unsafe mraa_uart_flush as mraaUartFlush
  {`UARTHandlePtr'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_set_baudrate as mraaUartSetBaudrate
  {`UARTHandlePtr', `CUInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_set_mode as mraaUartSetMode
  {`UARTHandlePtr', `CInt', `MRAA_UART_PARITY', `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_set_flowcontrol as mraaUartSetFlowcontrol
  {`UARTHandlePtr', `Bool', `Bool'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_set_timeout as mraaUartSetTimeout
  {`UARTHandlePtr', `CInt', `CInt', `CInt'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_get_dev_path as mraaUartGetDevPath
  {`UARTHandlePtr'} -> `String' #}

{#fun unsafe mraa_uart_stop as mraaUartStop
  {`UARTHandlePtr'} -> `MRAA_RESULT' #}

{#fun unsafe mraa_uart_read as mraaUartRead
  {`UARTHandlePtr', allocate32- `[CChar]' peek32*, `CUInt'} -> `CInt' #}

{#fun unsafe mraa_uart_write as mraaUartWrite
  {`UARTHandlePtr', withCharArray* `[CChar]', `CUInt'} -> `CInt' #}

{#fun unsafe mraa_uart_data_available as mraaUartDataAvailable
  {`UARTHandlePtr', `CUInt'} -> `Bool' #}

