$inlinecom { }


Scalars

ResMargin reserve margin /0.15/
FCASDem   FCAS requirement (raise and lower) as a % of hourly demand /0.03/
FCASREFrac FCAS requirement component coming from RE variability  /0.15/
MaxEmission  max emission factor in kg per MWh  /10000/
InitialStor  hour 1 storage /0.2/
;

Sets
g generation technology options /Coal, Solar, Nuke, Wind/

RE(g) renewable options /Solar,Wind/

t hours modeled /1*8760/

s RE scenarios /Avg, Low, High/

;

Parameter Prob(s) probability of RE scenarios /Avg 0.5, Low 0.2, High 0.3/ ;

Parameters GenData(g,*) generator data, Profile(t,*) RE profile, REUncert(g,s,*) RE uncertainties


$onecho > gdxxrwCM.in
par=GenData         rdim=1 cdim=1 rng=GenData!A2:Z70
par=Profile         rdim=1 cdim=1 rng=Profile!A1:G40000
$offecho

$call gdxxrw "DataIndia.xlsx" @gdxxrwCM.in MaxDupeErrors=1000
$gdxIn "DataIndia.gdx"
$load GenData Profile
$gdxIn

display gendata, profile;



Variables

Gen(g,s,t)   dispatch of each gen technology for each scenario for each hour
Cap(g)       optimal capacity decision for generation technology g
StorCap      optimal storage capacity
Storage(s,t) storage level - operation decision
StorInj(s,t) storage injection - operation decision
StorGen(s,t) storage discharge or generation - operation decision

RFCAS(g,s,t)  raise FCAS to manage low frequency
LFCAS(g,s,t)  lower FCAS to manage high frequency
StRFCAS(s,t)   raise FCAS from storage
StLFCAS(s,t)  lower FCAS from storage

Unmet(s,t)   unmet energy
Surplus(s,t) surplus energy

AvailCap(s,t)  available capacity

Emissions(s)   total CO2 emissions

Cost         system cost - objective function

;

positive variables cap, gen, storcap, storinj, storgen, rfcas, lfcas, strfcas, stLfcas, unmet, surplus, emissions;


RFCAS.fx(RE,s,t)=0;
LFCAS.fx(RE,s,t)=0;
RFCAS.fx("Nuke",s,t)=0;
LFCAS.fx("Nuke",s,t)=0;

Storage.fx(s,"1")=InitialStor;
*Storage.fx(s,"24")=7.5;

StorInj.fx(s,"1")=0;

*StorCap.fx = 0;

*Gen.fx("Coal",s,t) = 0;
*Gen.fx(RE,s,t) = 0;

*Gen.fx("Solar",s,t) = 0;



Equations

Demand(s,t)
StorBal(s,t)
StorLim(s,t)
StorGenCap(s,t)

CoalRamp1(s,t)
CoalRamp2(s,t)
CoalMinGen(s,t)

NukeRamp1(s,t)
NukeRamp2(s,t)
NukeMinGen(s,t)

JointFCAS(g,s,t)  Joint RFCAS and generation for coal
LFCAS_St(s,t)     Lower FCAS and discharge for storage

RFCASReq(s,t)      Raise FCAS requirement
LFCASReq(s,t)      Lower FCAS requirement

Profilecon1(RE,s,t)
Profilecon2(RE,s,t)
Profilecon3(RE,s,t)
Profilecon4(RE,s,t)
Profilecon5(RE,s,t)
Profilecon6(RE,s,t)

CapCon(g,s,t)
CapCon1(g,s)

FirmCapCon(s,t)

JointFCAS(g,s,t)

AvailCapEq1(s,t)
AvailCapEq2(s,t)
AvailCapEq3(s,t)

EmisCon(s)
EmisCon1(s)

Obj

;


FirmCapCon(s,t).. AvailCap(s,t) =g= Profile(t,"Load")*(1+ResMargin);

AvailCapEq1("Avg",t).. AvailCap("Avg",t) =e=
Cap("Coal")*0.95 + Cap("Nuke")*0.98 + Cap("Wind")*Profile(t,"Wind_Avg") + Cap("Solar")*Profile(t,"Solar_Avg") + StorCap*0.8;

AvailCapEq2("Low",t).. AvailCap("Low",t) =e=
Cap("Coal")*0.95 +  Cap("Nuke")*0.98 + Cap("Wind")*Profile(t,"Wind_Low") + Cap("Solar")*Profile(t,"Solar_Low") + StorCap*0.98;

AvailCapEq3("High",t).. AvailCap("High",t) =e=
Cap("Coal")*0.95 +  Cap("Nuke")*0.98 + Cap("Wind")*Profile(t,"Wind_High") + Cap("Solar")*Profile(t,"Solar_High") + StorCap*0.98;

Demand(s,t).. Sum(g,Gen(g,s,t)) + StorGen(s,t)*0.9 + Unmet(s,t) - Surplus(s,t) =e=
Profile(t,"Load")+StorInj(s,t)  ;


StorBal(s,t)$(ord(t)>1).. Storage(s,t) =e= Storage(s,t-1) + StorInj(s,t) - StorGen(s,t);


CoalRamp1(s,t)$(ord(t)>1).. Gen("Coal",s,t) =l= Gen("Coal",s,t-1)*1.5;

CoalRamp2(s,t)$(ord(t)>1).. Gen("Coal",s,t) =g= Gen("Coal",s,t-1)*0.5;

CoalMinGen(s,t).. Gen("Coal",s,t) =g= LFCAS("coal",s,t) + Cap("Coal")*0.4;

NukeRamp1(s,t)$(ord(t)>1).. Gen("Nuke",s,t) =l= Gen("Nuke",s,t-1)*1.2;

NukeRamp2(s,t)$(ord(t)>1).. Gen("Nuke",s,t) =g= Gen("Nuke",s,t-1)*0.8;

NukeMinGen(s,t).. Gen("Nuke",s,t) =g= LFCAS("Nuke",s,t) + Cap("Nuke")*0.8;

StorLim(s,t).. Storage(s,t) =l= StorCap;

StorGenCap(s,t).. StorGen(s,t) + StRFCAS(s,t) =l= Storage(s,t);

ProfileCon1("Wind","Avg",t).. Gen("Wind","avg",t) =e= Cap("Wind")*Profile(t,"Wind_Avg");
ProfileCon2("Wind","High",t).. Gen("Wind","high",t) =e= Cap("Wind")*Profile(t,"Wind_High");
ProfileCon3("Wind","Low",t).. Gen("Wind","Low",t) =e= Cap("Wind")*Profile(t,"Wind_Avg")*0.8;

ProfileCon4("Solar","Avg",t).. Gen("Solar","avg",t) =e= Cap("Solar")*Profile(t,"Solar_Avg");
ProfileCon5("Solar","High",t).. Gen("Solar","high",t) =e= Cap("Solar")*Profile(t,"Solar_High");
ProfileCon6("Solar","Low",t).. Gen("Solar","Low",t) =e= Cap("Solar")*Profile(t,"Solar_Avg")*0.9;


JointFCAS(g,s,t).. Gen(g,s,t) + RFCAS(g,s,t) =l= Cap(g);

LFCAS_St(s,t)..    StLFCAS(s,t) =l= StorCap - Storage(s,t);

RFCASReq(s,t).. Sum(g, RFCAS(g,s,t)) + StRFCAS(s,t) =g= Profile(t,"Load")*FCASDem + Sum(RE, Gen(RE,s,t))*FCASREFrac;

LFCASReq(s,t).. Sum(g, LFCAS(g,s,t)) + StLFCAS(s,t) =g= Profile(t,"Load")*FCASDem + Sum(RE, Gen(RE,s,t))*FCASREFrac;


CapCon(g,s,t).. Gen(g,s,t) =l= Cap(g);

CapCon1(g,s).. Sum(t, Gen(g,s,t)) =l= Cap(g)*Card(t)*0.9;


EmisCon(s).. Emissions(s) =e= Sum(t, Gen("coal",s,t)*0.9) ;

EmisCon1(s).. Emissions(s) =l= MaxEmission ;



Obj.. Cost =e= Sum((t,s,g), Gen(g,s,t)*GenData(g,"opex")*Prob(s))
             + Sum(g,Cap(g)*GenData(g,"AnnCapex"))
             + Sum(g,Cap(g)*GenData(g,"FOM"))
             + StorCap*37 {$250/kWh capex} +StorCap*10 {4% FOM}
             + Sum((s,t), (Unmet(s,t)*1*Prob(s) + Surplus(s,t)*0.05*Prob(s)))
             + Sum((t,s,g), RFCAS(g,s,t)+LFCAS(g,s,t))*0.005
             + Sum((t,s), STRFCAS(s,t)+STLFCAS(s,t))*0.004
;

Model India /all/ ;

India.optfile = 1;

Solve India us LP min cost;

display cap.l, storcap.l, gen.l, storinj.l, storgen.l, unmet.l, surplus.l

Parameter TotalCapex;

TotalCapex =               (+ Sum(g,Cap.l(g)*GenData(g,"AnnCapex")*1000)
             + StorCap.l*37000)/1000;

display totalcapex, CoalRamp1.m, CoalRamp2.m;
;

Parameter Summary(*,*), SummaryGen(g,s), Emission(s,*), FCASSum(s,*);

Option decimals=0;

Summary ("Nuke","Cap kW") = Cap.l("Nuke") ;
Summary ("Coal","Cap kW") = Cap.l("Coal") ;
Summary ("Solar","Cap kW") = Cap.l("Solar") ;

Summary ("Wind","Cap kW") = Cap.l("Wind") ;
Summary ("Storage","Cap kWh") = StorCap.l ;

Summary ("Nuke","Cap. ex") = Cap.l("Nuke")*GenData("Nuke","Capex") ;
Summary ("Coal","Cap. ex") = Cap.l("Coal")*GenData("Coal","Capex") ;
Summary ("Solar","Cap. ex") = Cap.l("Solar")*GenData("Solar","Capex") ;
Summary ("Wind","Cap. ex") = Cap.l("Wind")*GenData("Wind","Capex") ;
Summary ("Storage","Cap. ex") = StorCap.l*250 ;
Summary ("Total","Cap. ex") = Summary ("Nuke","Cap. ex")+Summary ("Coal","Cap. ex")+ Summary ("Solar","Cap. ex")+Summary ("Wind","Cap. ex")+Summary ("Storage","Cap. ex");

Summary ("Nuke","Ann. Capex") = Cap.l("Nuke")*GenData("Nuke","AnnCapex") ;
Summary ("Coal","Ann. Capex") = Cap.l("Coal")*GenData("Coal","AnnCapex") ;
Summary ("Solar","Ann. Capex") = Cap.l("Solar")*GenData("Solar","AnnCapex") ;
Summary ("Wind","Ann. Capex") = Cap.l("Wind")*GenData("Wind","AnnCapex") ;
Summary ("Storage","Ann. Capex") = StorCap.l*37 ;
Summary ("Total","Ann. Capex") = Summary ("Nuke","Ann. Capex")+Summary ("Coal","Ann. Capex")+ Summary ("Solar","Ann. Capex")+Summary ("Wind","Ann. Capex")+Summary ("Storage","Ann. Capex");

Summary ("Nuke","Fixed O&M") = Cap.l("Nuke")*GenData("Nuke","FOM") ;
Summary ("Coal","Fixed O&M") = Cap.l("Coal")*GenData("Coal","FOM") ;
Summary ("Solar","Fixed O&M") = Cap.l("Solar")*GenData("Solar","FOM") ;
Summary ("Wind","Fixed O&M") = Cap.l("Wind")*GenData("Wind","FOM") ;
Summary ("Storage","Fixed O&M") = StorCap.l*10 ;
Summary ("Total","Fixed O&M") = Summary ("Nuke","Fixed O&M")+Summary ("Coal","Fixed O&M")+ Summary ("Solar","Fixed O&M")+Summary ("Wind","Fixed O&M")+Summary ("Storage","Fixed O&M");

Summary ("Nuke","Opex") = Sum((s,t), Gen.l("Nuke",s,t)*GenData("Nuke","opex")/card(s)) ;
Summary ("Coal","Opex") = Sum((s,t), Gen.l("Coal",s,t)*GenData("Coal","opex")/card(s)) ;
Summary ("Total","Opex") = Summary ("Coal","Opex")+Summary ("Nuke","Opex");


Summary ("Total","Unserved Cost") = Sum((s,t), (Unmet.l(s,t)*1*Prob(s) ));
Summary ("Total","Surplus Cost") = Sum((s,t), ( Surplus.l(s,t)*0.05*Prob(s)));

Summary ("Total","FCAS Cost")   =
             + Sum((t,s,g), RFCAS.l(g,s,t)+LFCAS.l(g,s,t))*0.005
             + Sum((t,s), STRFCAS.l(s,t)+STLFCAS.l(s,t))*0.004;



Summary ("Total","Ann. Cost") =
Summary ("Coal","Opex") +Summary ("Nuke","Opex")+Summary ("Total","Fixed O&M")+ Summary ("Total","Ann. Capex")
+ Summary("Total","Unserved Cost") + Summary("Total","Surplus Cost") + Summary ("Total","FCAS Cost");


SummaryGen(g,s) = Sum(t, Gen.l(g,s,t))/1000;


Emission(s,"Coal MWh") = Sum(t, Gen.l("coal",s,t))/1000;

Emission(s,"Emissions") = Emissions.l(s);
Emission(s,"Marginal CO2 cost $/t") = EmisCon1.m(s);


FCASSum(s,"Raise/Lower Req") = Sum(t, Profile(t,"Load")*FCASDem + Sum(RE, Gen.l(RE,s,t))*FCASREFrac);

FCASSum(s,"Coal RFCAS") = Sum(t, Sum(g, RFCAS.l(g,s,t)));
FCASSum(s,"Coal LFCAS") = Sum(t, Sum(g, LFCAS.l(g,s,t)));

FCASSum(s,"Storage RFCAS") = Sum(t, STRFCAS.l(s,t));
FCASSum(s,"Storage LFCAS") = Sum(t, STLFCAS.l(s,t));

FCASSum(s,"RFCAS Price") = Sum(t, RFCASReq.m(s,t))/card(t);
FCASSum(s,"LFCAS Price") = Sum(t, LFCASReq.m(s,t))/card(t);

FCASSum(s,"Energy Price") = Sum(t, Demand.m(s,t))/card(t);



display summary;

Parameter Viol(s,t,*), StorageDat(s,t,*) ;

Viol(s,t,"unmet")=unmet.l(s,t);
Viol(s,t,"surplus")=surplus.l(s,t);

StorageDat(s,t,"Previous t") = Storage.l(s,t-1);
StorageDat(s,t,"Injection") = StorInj.l(s,t);
StorageDat(s,t,"Discharge") = -StorGen.l(s,t);
StorageDat(s,t,"Level") = Storage.l(s,t);



execute_unload 'RESULTS.gdx', summary, summarygen, FCASSum, emission, gen.l, StorageDat,viol;

* Write the data from the GDX file to the Excel file using gdxxrw
execute 'gdxxrw RESULTS.gdx par=Summary rng=Summary!A5 par=SummaryGen rng=SummaryGen!A5 par=FCASSum rng=FCASSum!A5 par=Emission rng=Emission!A5 var=Gen rng=Gen!A5  par=StorageDat rng=Storage!A5  par=Viol rng=Unmet_Surplus!A5';

Option decimals=2;

display firmcapcon.m;

display rfcas.l, lfcas.l, RFCASReq.m, LFCASReq.m, emissions.l, emiscon1.m;
