#ifndef __EL_MASTER_API_H
#define __EL_MASTER_API_H
#include <stdint.h>
#include <stdbool.h>

/*!
 *  Sized types for "hardware" objects
 */

typedef uint8_t     ElMasterTag;
typedef uint64_t    ElMasterWord;
typedef uint32_t    ElMasterRamAddress;
typedef uint16_t    ElMasterCpuMask;
typedef uint8_t     ElMasterIomMask;
typedef uint32_t    ElMasterRamModulesMask;
typedef uint8_t     ElMasterProcessorIndex;

/*!
 *  Status codes
 */

typedef enum
{
    EMS_OK                          = 0x00,
    EMS_ERROR_INVALID_ARGUMENT      = 0x01,
    EMS_ERROR_INVALID_ADDRESS       = 0x02,
    EMS_ERROR_RAM_MODULE_NOT_FOUND  = 0x03,
    EMS_INVALID_STATE               = 0x04,
}                   ElMasterStatus;

/*!
 *  Interface functions
 */

/*!
 * Read a word with tag from RAM.
 *
 * @param[in]   address     RAM address
 * @param[out]  pTag        word tag
 * @param[out]  pWord       word value
 *
 * @return      EMS_OK
 *              EMS_ERROR_INVALID_ARGUMENT      pWord/pTag is not a valid pointer
 *              EMS_ERROR_INVALID_ADDRESS       address >= 2**20
 *              EMS_ERROR_RAM_MODULE_NOT_FOUND  RAM module not configured
 */
ElMasterStatus
elMasterRamWordRead
(
    ElMasterRamAddress  address,
    ElMasterTag         *pTag,
    ElMasterWord        *pWord
);

/*!
 * Write a word with tag to RAM.
 *
 * @param[in]   address     RAM address
 * @param[in]   tag         word tag
 * @param[in]   word        word value
 *
 * @return      EMS_OK
 *              EMS_ERROR_INVALID_ADDRESS       address >= 2**20
 *              EMS_ERROR_RAM_MODULE_NOT_FOUND  RAM module not configured
 */
ElMasterStatus
elMasterRamWordWrite
(
    ElMasterRamAddress  address,
    ElMasterTag         tag,
    ElMasterWord        word
);

/*!
 * Read a word with tag from RAM, atomically imposing the Lock Bit in the RAM.
 * Return the original value.
 *
 * @param[in]   address     RAM address
 * @param[out]  pTag        word tag
 * @param[out]  pWord       word value
 *
 * @return      EMS_OK
 *              EMS_ERROR_INVALID_ARGUMENT      pWord/pTag is not a valid pointer
 *              EMS_ERROR_INVALID_ADDRESS       address >= 2**20
 *              EMS_ERROR_RAM_MODULE_NOT_FOUND  RAM module not configured
 */
ElMasterStatus
elMasterRamWordReadWithLock
(
    ElMasterRamAddress  address,
    ElMasterTag         *pTag,
    ElMasterWord        *pWord
);

/*!
 * Query CPU's/IOM's own index
 *
 * @return      CPU/IOM index, 0..9 for CPUs, 0..3 for IOMs
 */
ElMasterProcessorIndex
elMasterGetIndex
(
    void
);

/*!
 * Join or leave the master configuration.
 *
 * @param[in]   bJoin   true    join
 *                      false   leave
 *
 * @return      EMS_OK
 *              EMS_INVALID_STATE               request is not appropriate
 *                                              in the current context
 */
ElMasterStatus
elMasterConfigJoinLeave
(
    bool    bJoin
);

/*!
 * Query the present hardware masks. Any pointer can be passed as NULL.
 *
 * @param[out]  pCpuMask        bit 0 => CPU 0 .. bit 9 => CPU 9
 * @param[out]  pIomMask        bit 0 => IOM 0 .. bit 3 => IOM 3
 * @param[out]  pRamModulesMask bit 0 => section 0 / module 0 .. bit 31 => section 7 / module 3
 *
 * @return      EMS_OK
 */
ElMasterStatus
elMasterGetMasterConfig
(
    ElMasterCpuMask         *pCpuMask,
    ElMasterIomMask         *pIomMask,
    ElMasterRamModulesMask  *pRamModulesMask
);

/*!
 * Send an interrupt signal to CPUs and/or IOMs.
 *
 * @param[in]  cpuMask        bit 0 => CPU 0 .. bit 9 => CPU 9
 * @param[in]  iomMask        bit 0 => IOM 0 .. bit 3 => IOM 3
 *
 * @return      EMS_OK
 */
ElMasterStatus
elMasterSendInterrupt
(
    ElMasterCpuMask cpuMask,
    ElMasterIomMask iomMask
);

/*!
 * Send a response signal to CPUs and/or IOMs.
 *
 * @param[in]  cpuMask        bit 0 => CPU 0 .. bit 9 => CPU 9
 * @param[in]  iomMask        bit 0 => IOM 0 .. bit 3 => IOM 3
 *
 * @return      EMS_OK
 */
ElMasterStatus
elMasterSendResponse
(
    ElMasterCpuMask cpuMask,
    ElMasterIomMask iomMask
);

/*!
 * Enter or leave the fault state.
 *
 * @param[in]   bEnter  true    enter
 *                      false   leave
 *
 * @return      EMS_OK
 *              EMS_INVALID_STATE               request is not appropriate
 *                                              in the current context
 */
ElMasterStatus
elMasterFaultEnterLeave
(
    bool    bEnter
);

/*!
 * Register a callback function that would receive notifications about
 * the master signals vector change. The interrupt and response signals
 * are edge-triggered, fault condition is level triggered.
 * The callback must be registered before a call to elMasterConfigJoinLeave(true);
 *
 * @param[in]   pCallback   pointer to the callback function
 *
 * @return      EMS_OK
 *              EMS_ERROR_INVALID_ARGUMENT      pCallback is not a valid pointer
 */
struct ElMasterSignals
{
    ElMasterCpuMask cpuInterruptMask;
    ElMasterCpuMask cpuResponseMask;
    ElMasterCpuMask cpuFaultMask;
    ElMasterIomMask iomInterruptMask;
    ElMasterIomMask iomResponseMask;
    ElMasterIomMask iomFaultMask;
};

typedef void    ElMasterCallback(struct ElMasterSignals *);

ElMasterStatus
elMasterRegisterCallback
(
    ElMasterCallback    *pCallback
);

#endif // __EL_MASTER_API_H
