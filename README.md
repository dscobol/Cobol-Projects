This is code I have created or copied and modified as I progress learning COBOL.

There are two main areas:

    OpenCobol: These are programs that were written to use the gnuCobol compiler

    Mainframe: These are programs that were written to run on IBM's MVS 3.8j and Enterprise zOS.

There will be quite a few programs that are the same but with slight modifications for the different environments they will run in.

The Mainframe area is divided into two areas:

    MVS: These are created and ran on a TK4-/Hercules Emulator running on a Raspberry Pi.

    ZOS: They will be run on a Z14? being provided by IBM as part of their [Learn to code in COBOL](https://www.openmainframeproject.org/projects/coboltrainingcourse) program.

The ZOS programs should be the same as the programs in the MVS area but to be safe, they will be separated.

The most probable changes are VSAM, and CICS. 

I can't run CICS on ZOS (I don't have access to it) but I can and am running VSAMIO and KIKS on the TK4- system.

There are also two different "areas" under the ZOS directory.

    Normal: This is as it sounds. I create the Source, Copybooks and JCL and then upload it and run it.

    [Zowe](https://www.zowe.org/): From their site: "Zowe is an open source project created to host technologies that benefit the Z platform". It's an API that allows you to interface with the Mainframe in a couple of different ways. It's kinda neat! 

