# JDS-GTM
JDS (OSEHRA Release of eHMP) with GT.M support

# Authors
Christopher Edwards, David Wicksell.

GT.M modifications - Sam Habiel.

# Configuration Instructions
Import all routines into Cache or GT.M. If you use Cache, make sure that the %ut namespace has been mapped to JDS.

Run the following:

```
D SETUP^VPRJCONFIG
D GO^VPRJRCL
```

That's it. JDS is ready to use at this point. You can test ping by hitting `http://<ip address>:9080/ping`.
