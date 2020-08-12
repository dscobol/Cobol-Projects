This is code I have created or copied and modified as I progress learning COBOL.

There are two main areas:

    OpenCobol: These are programs that were written to use the gnuCobol compiler

    Mainframe: These are programs that were written to run on IBM's MVS 3.8j and Enterprise zOS.

There will be quite a few programs that are the same but with slight modifications for the different environments they will run in.

The Mainframe area is divided into two areas:

MVS: These are created and ran on a TK4-/Hercules Emulator running on a Raspberry Pi.

ZOS: They will be run on a Z14? being provided by IBM as part of 
their [Learn to code in COBOL](https://www.openmainframeproject.org/projects/coboltrainingcourse) program.

The ZOS programs will be close to the MVS programs but there are sometimes small differences in both source code and jcl so they will be tracked seperately.

One huge difference between the two areas are VSAM, and CICS. 

I can't run CICS on ZOS (I don't have access to it) but I can and am running VSAMIO and KIKS on the TK4- system.

There are two users under the MVS directory: HERC01 and HERC03.

HERC01 is the "System Admin" account.

HERC03 is a "normal" user and all code that I am creating and sharing between the different environments will be found here.

There are three different "areas" under the ZOS directory.

Normal: This is as it sounds. I create the Source, Copybooks and JCL and then upload it and run it.

ECBAP: This is the [Enterprise COBOL for Business Application Programming](https://community.ibm.com/community/user/ibmz-and-linuxone/viewdocument/enterprise-cobol-for-business-appli?CommunityKey=b0dae4a8-74eb-44ac-86c7-90f3cd32909a&tab=librarydocuments) class offered by IBM.

[Zowe](https://www.zowe.org/): From their site: "Zowe is an open source project created to host technologies that benefit the Z platform". 
It's an API that allows you to interface with the Mainframe in a couple of different ways. It's kinda neat! 
