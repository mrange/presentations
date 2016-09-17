namespace AttributeQueryCs.Original
{
  using System;
  using System.Xml;
  using System.Collections.Generic;
  using Common;

  static class QueryContext
  {
    public static XmlDocument   ProdDataXml ;
    public static string[]      SysDataParam;
    public static string[]      Gpp3Data    ;
    public static List<string>  Errors      ;
  }

  static class AttributeQuery
  {
    public static Func<U> Then<T, U> (this Func<T> t, Func<T, U> u)
    {
      return () => u(t ());
    }

    public static Func<string> Iwd<T> (T v)
    {
      return () => v.ToString ();
    }

    public static Func<string> Gpp3 (string path, string key)
    {
      return () => "TODO:";
    }

    public static Func<string> Gpp3 (string path, Func<string> key)
    {
      return () => "TODO:";
    }

    public static Func<string> ProdDataXml (string path)
    {
      return () => "TODO:";
    }

    public static Func<string> SysDataParam (string path)
    {
      return () => "TODO:";
    }

    public static Func<string, string> Select (int idx)
    {
      return v => v;
    }
  }

  sealed class AttributeDescriptor
  {
    public readonly string                  Name      ;
    public readonly IUnitConverter          Converter ;
    public readonly Func<string>            Query     ;


    public AttributeDescriptor (string name, IUnitConverter converter, Func<string> query)
    {
      Name      = name      ?? ""                         ;
      Converter = converter ?? UnitConverters.Scalar      ;
      Query     = query     ?? AttributeQuery.Iwd ("")    ;
    }
  }

  sealed class CapabilityDescriptor
  {
    public readonly string                Name      ;
    public readonly AttributeDescriptor[] Attributes;

    public CapabilityDescriptor (string name, params AttributeDescriptor[] attributes)
    {
      Name        = name        ?? ""                         ;
      Attributes  = attributes  ?? new AttributeDescriptor[0] ;
    }
  }
}

namespace AttributeQueryCs.Original
{
  using System;
  using Common;

  using static AttributeQuery;
  using static Common.UnitConverters;

  static class Capabilities
  {
    static AttributeDescriptor A (string name, IUnitConverter converter, Func<string> query)
    {
      return new AttributeDescriptor (name, converter, query);
    }

    static CapabilityDescriptor C (string name, params AttributeDescriptor[] attributes)
    {
      return new CapabilityDescriptor (name, attributes);
    }

    static CapabilityDescriptor[] CreateCapabilities ()
    {
      var dBm0_1          = Powers.dBm0_1;
      var kHz             = Frequencies.kHz;

      var freqClassUsage  = ProdDataXml ("/board/freqClassUsage");

      return new []
      {
          C ("CRBS_TRS_CAP_CARDINALITY_SUPPORT"
            , A ("capabilityIdentity"       , Scalar                                                , Iwd(1))
            , A ("numberOfDevices"          , Scalar                                                , Iwd(0))  /* IWD: "Number of static devices" */
            )
         , C ("CRBS_TRS_CAP_RF_CHAR_SUPPORT"
            , A ("capabilityIdentity"       , Scalar                                                , Iwd(2))
            , A ("txPortMaximumOutputPower" , Power (dBm0_1 /* IWD */ , dBm0_1  /* not always! */)  , ProdDataXml ("/board/powerClassUsage"))
            , A ("txPortMaximumPar"         , Power (dBm0_1 /* IWD */ , dBm0_1  /* sysDataParam */) , SysDataParam ("capMaxPar"))
            , A ("txOperationBandLowEdge"   , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , Gpp3 ("bandLimits", freqClassUsage).Then (Select (0))) /* select "DL_f_min" */
            , A ("txOperationBandHighEdge"  , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , Gpp3 ("bandLimits", freqClassUsage).Then (Select (1))) /* select "DL_f_max" */
            , A ("rxOperationBandLowEdge"   , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , Gpp3 ("bandLimits", freqClassUsage).Then (Select (2))) /* select "UL_f_min" */
            , A ("rxOperationBandHighEdge"  , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , Gpp3 ("bandLimits", freqClassUsage).Then (Select (3))) /* select "UL_f_max" */
            , A ("duplexMode"               , Scalar                                                , SysDataParam ("capabilityDuplexMode"))
            , A ("txMaximumBandwidth"       , Frequency (kHz /* IWD */, kHz /* match IWD range */)  , SysDataParam ("capabilityDlBw")) /* Observed values in sysDataParam 10000-75000*/
            , A ("rxMaximumBandwidth"       , Frequency (kHz /* IWD */, kHz /* match IWD range */)  , SysDataParam ("capabilityUlBw")) /* Observed values in sysDataParam 10000-75000*/
            )
     };
    }

    public static readonly CapabilityDescriptor[] AllCapabilities = CreateCapabilities ();
  }
}
