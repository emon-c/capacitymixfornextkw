$inlinecom { }

Scalars
CoalRamp /0.5/
GasRamp /0.5/
EmissionsCap /10000/
DatacenterRatio fraction of load to scale datacenter demand /0.2/

Sets
g /Coal, Solar, Wind, Gas/

RE(g) /Solar,Wind/

t /1*24/

s RE scenarios /Avg, Low, High/ {I am just going to assume High=1.2*Avg and Low=0.8*Avg}

;

Parameters GenData(g,*), Profile(t,*), REUncert(g,s,*)


$onecho > gdxxrwCM.in
par=GenData         rdim=1 cdim=1 rng=GenData!A2:Z70
par=Profile         rdim=1 cdim=1 rng=Profile!A2:E400
par=REUncert        rdim=2 cdim=1 rng=REUncert!A3:D100
$offecho

$call gdxxrw "China.xlsx" @gdxxrwCM.in MaxDupeErrors=1000
$gdxIn "China.gdx"
$load GenData Profile REUncert
$gdxIn

display gendata, profile, reuncert;



Variables

Gen(g,s,t)

GenRE1(g,s,t)
GenRE2(g,s,t)
GenRETotal(g,s,t)

Cap(g)
GenStorCap
GreenStorCap

StorGreenInj(s,t) Power coming into Green battery
StorGreenOut(s,t) Power exiting Green battery
StorGreen(s,t)    Power within Green battery

StorGenInj(s,t)   Power coming into General battery
StorGenOut(s,t)   Power exiting General battery
StorageGeneral(s,t) Power within General battery

Unmet(t)
Surplus(t)

Unmet1(t)
Surplus1(t)

Cost

;

positive variables cap, gen, storcap, storinj, storgreen, storagegeneral, storgeninj, storgenout, storgreeninj;
positive variables storgreenout, unmet, surplus, unmet1, surplus1, GenRE1, GenRE2, GenRETotal;

StorageGeneral.fx(s,"1")=0.125;
StorageGeneral.fx(s,"24")=0.125;

StorGreen.fx(s,"1")=0.125;
StorGreen.fx(s,"24")=0.125;

StorGenInj.fx(s,"1")=0;
StorGreenInj.fx(s,"1")=0;




GenRETotal.fx("Coal",s,t) = 0;
GenRETotal.fx("Gas",s,t) = 0;

Gen.fx(RE,s,t) = 0;

*StorCap.fx = 0;

*Gen.fx("Coal",s,t) = 0;
*Cap.fx("Solar") = 1;

*Gen.fx("Wind",s,t) = 0;

Gen.fx(g,s,t)$(REUncert(g,s,"Prob")=0) = 0;


Equations

Demand(s,t)
GenREBalance(g,s,t)
GreenDemand(s,t)
StorBalGeneral(s,t)
StorBalGreen(s,t)
StorLimGen(s,t)
StorLimGreen(s,t)
StorGenCap1(s,t)
StorGenCap2(s,t)
CoalRamp1(s,t)
CoalRamp2(s,t)
CoalMinGen(s,t)
GasRamp1(s,t)
GasRamp2(s,t)
GasMinGen(s,t)
Profilecon(RE,s,t)
CapCon(g,s,t)
CapCon1(g,s)
Emissions(s)
Obj

;

*Demand for the Datacenter must be met using renewable sources + the battery
*Energy from the battery going into the datacenter must be green
*The generation going to the datacenter can't overlap with the generation going to the grid
*

Demand(s,t)$(Sum(g,REUncert(g,s,"Prob"))>0).. Sum(g,GenRE1(g,s,t) + Gen(g,s,t)) + StorGenOut(s,t)*0.9  + Unmet(t) - Surplus(t) =e=
Profile(t,"Load") + StorGenInj(s,t) ;


GenREBalance(g,s,t).. GenRETotal(g,s,t) =e= GenRE1(g,s,t) + GenRE2(g,s,t);


GreenDemand(s,t)$(Sum(g,REUncert(g,s,"Prob"))>0).. Sum(g,GenRE2(g,s,t)) + StorGreenOut(s,t)*0.9 + Unmet1(t) - Surplus1(t)  =e=
Profile(t,"Datacenter")*DatacenterRatio + StorGreenInj(s,t) ;


StorBalGeneral(s,t)$(ord(t)>1).. StorageGeneral(s,t) =e= StorageGeneral(s,t-1)
                            + StorGenInj(s,t) - StorGenOut(s,t);
                            

StorBalGreen(s,t)$(ord(t)>1).. StorGreen(s,t) =e= StorGreen(s,t-1)
                            + StorGreenInj(s,t) - StorGreenOut(s,t);



CoalRamp1(s,t).. Gen("Coal",s,t) =l= Gen("Coal",s,t)*(1 + CoalRamp);

CoalRamp2(s,t).. Gen("Coal",s,t) =g= Gen("Coal",s,t)*(1 - CoalRamp);

CoalMinGen(s,t).. Gen("Coal",s,t) =g= Cap("Coal")*0.4;



GasRamp1(s,t).. Gen("Gas",s,t) =l= Gen("Gas",s,t)*(1 + GasRamp);

GasRamp2(s,t).. Gen("Gas",s,t) =g= Gen("Gas",s,t)*(1 - GasRamp);

GasMinGen(s,t).. Gen("Gas",s,t) =g= Cap("Gas")*0.4;


StorLimGen(s,t).. StorageGeneral(s,t) =l= GenStorCap;
StorLimGreen(s,t).. StorGreen(s,t) =l= GreenStorCap;


StorGenCap1(s,t).. StorGenOut(s,t) =l= StorageGeneral(s,t);
StorGenCap2(s,t).. StorGreenOut(s,t) =l= StorGreen(s,t);

ProfileCon(RE,s,t).. GenRETotal(RE,s,t) =e= Cap(RE)*Profile(t,RE)*REUncert(RE,s,"Scale");

CapCon(g,s,t).. Gen(g,s,t) =l= Cap(g);

CapCon1(g,s).. Sum(t, Gen(g,s,t)) =l= Cap(g)*Card(t)*0.9;

Emissions(s).. Sum((t,g), GenData(g,"Emissions")*Gen(g,s,t))*365 =l= EmissionsCap ;

Obj.. Cost =e= Sum((t,s,g), Gen(g,s,t)*GenData(g,"opex")*365*REUncert(g,s,"Prob") + GenRETotal(g,s,t)*GenData(g,"opex")*365*REUncert(g,s,"Prob"))
             + Sum(g,Cap(g)*GenData(g,"AnnCapex"))
             + Sum(g,Cap(g)*GenData(g,"FOM"))
             + GenStorCap*27 + GenStorCap*7 + GreenStorCap*27 + GreenStorCap*7
             + Sum((t), ((Unmet(t) + Unmet1(t))*1 + (Surplus(t)+Surplus1(t))*0.05))*365
;

Model China /all/ ;

Solve China us LP min cost;

display cap.l, genstorcap.l, greenstorcap.l, gen.l, storgeninj.l, storgenout.l,storgreeninj.l, storgreenout.l, unmet.l, surplus.l, unmet1.l, surplus1.l, GenRE1.l, GenRE2.l;

Parameter TotalCapex, TotalEmissions(s);

TotalCapex =               (+ Sum(g,Cap.l(g)*GenData(g,"AnnCapex")*1000)
             + (GenStorCap.l + GreenStorCap.l)*27000)/1000;
             
TotalEmissions(s) = Sum((t,g), GenData(g,"Emissions")*Gen.l(g,s,t))*365;

display totalcapex, totalemissions, emissions.m;

Parameter Summary(*,*);

Option decimals=0;

Summary ("Coal","Cap kW") = Cap.l("Coal") ;
Summary ("Gas","Cap kW") = Cap.l("Gas") ;
Summary ("Solar","Cap kW") = Cap.l("Solar") ;
Summary ("Wind","Cap kW") = Cap.l("Wind") ;
Summary ("General Storage","Cap kWh") = GenStorCap.l ;
Summary ("Green Storage","Cap kWh") = GreenStorCap.l ;


Summary ("Coal","Cap. ex") = Cap.l("Coal")*GenData("Coal","Capex") ;
Summary ("Gas","Cap. ex") = Cap.l("Gas")*GenData("Gas","Capex") ;
Summary ("Solar","Cap. ex") = Cap.l("Solar")*GenData("Solar","Capex") ;
Summary ("Wind","Cap. ex") = Cap.l("Wind")*GenData("Wind","Capex") ;
Summary ("Storage","Cap. ex") = (GenStorCap.l + GreenStorCap.l)*180 ;
Summary ("Total","Cap. ex") = Summary ("Coal","Cap. ex") + Summary ("Gas","Cap. ex") + Summary ("Solar","Cap. ex")+Summary ("Wind","Cap. ex")+Summary ("Storage","Cap. ex");

Summary ("Coal","Ann. Capex") = Cap.l("Coal")*GenData("Coal","AnnCapex") ;
Summary ("Gas","Ann. Capex") = Cap.l("Gas")*GenData("Gas","AnnCapex") ;
Summary ("Solar","Ann. Capex") = Cap.l("Solar")*GenData("Solar","AnnCapex") ;
Summary ("Wind","Ann. Capex") = Cap.l("Wind")*GenData("Wind","AnnCapex") ;
Summary ("Storage","Ann. Capex") = (GenStorCap.l + GreenStorCap.l)*27 ;
Summary ("Total","Ann. Capex") = Summary ("Coal","Ann. Capex") + Summary ("Gas","Ann. Capex") + Summary ("Solar","Ann. Capex")+Summary ("Wind","Ann. Capex")+Summary ("Storage","Ann. Capex");

Summary ("Coal","Fixed O&M") = Cap.l("Coal")*GenData("Coal","FOM") ;
Summary ("Gas","Fixed O&M") = Cap.l("Gas")*GenData("Gas","FOM") ;
Summary ("Solar","Fixed O&M") = Cap.l("Solar")*GenData("Solar","FOM") ;
Summary ("Wind","Fixed O&M") = Cap.l("Wind")*GenData("Wind","FOM") ;
Summary ("Storage","Fixed O&M") = (GenStorCap.l + GreenStorCap.l)*7 ;
Summary ("Total","Fixed O&M") = Summary ("Coal","Fixed O&M") + Summary ("Gas","Fixed O&M") + Summary ("Solar","Fixed O&M")+Summary ("Wind","Fixed O&M")+Summary ("Storage","Fixed O&M");

Summary ("Coal","Opex") = Sum((s,t), Gen.l("Coal",s,t)*GenData("Coal","opex")*365*REUncert("Coal",s,"Prob")) ;
Summary ("Total","Opex") = Summary ("Coal","Opex");

Summary ("Gas","Opex") = Sum((s,t), Gen.l("Gas",s,t)*GenData("Gas","opex")*365*REUncert("Gas",s,"Prob")) ;
Summary ("Total","Opex") = Summary ("Gas","Opex");


Summary ("Total","Ann. Cost") = Summary ("Coal","Opex") + Summary ("Gas","Opex") +Summary ("Total","Fixed O&M")+ Summary ("Total","Ann. Capex");


display summary;

execute_unload 'RESULTS.gdx', summary, gen.l, storgenout.l, storagegeneral.l;

* Write the data from the GDX file to the Excel file using gdxxrw
execute 'gdxxrw RESULTS.gdx par=Summary rng=Summary!A5 var=Gen rng=Gen!A5  var=StorageGeneral rng=Storage_Discharge!A5';
