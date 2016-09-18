namespace AttributeQueryCs.Monad
{
  using Common;

  delegate T AttributeQuery<T> (QueryContext ctx);
  delegate AttributeQuery<U> AttributeQueryKleisli<T, U> (T v);

  static class AttributeQuery
  {
    public static AttributeQuery<T> Return<T> (T v)
    {
      return ctx => v;
    }

    public static AttributeQuery<U> Bind<T, U> (this AttributeQuery<T> t, AttributeQueryKleisli<T, U> uf)
    {
      return ctx =>
        {
          var tv = t (ctx);
          var u  = uf (tv);
          return u (ctx);
        };
    }

    public static AttributeQuery<string> Iwd<T> (T v)
    {
      return ctx => v.ToString ();
    }

    public static AttributeQuery<string> Gpp3 (string path, string key)
    {
      return ctx => ctx.Gpp3Data.Get (path, key);
    }

    public static AttributeQueryKleisli<string, string> Gpp3 (string path)
    {
      return v => Gpp3 (path, v);
    }

    public static AttributeQuery<string> ProdDataXml (string path)
    {
      return ctx => ctx.ProductData.Get (path);
    }

    public static AttributeQuery<string> SysDataParam (string path)
    {
      return ctx =>  ctx.SysData.Get (path);
    }

    public static AttributeQuery<string> Select (int idx, string v)
    {
      return ctx => v.Split (',')[idx];
    }

    public static AttributeQueryKleisli<string, string> Select (int idx)
    {
      return v => Select (idx, v);
    }
  }

  sealed class AttributeDescriptor
  {
    public readonly string                  Name      ;
    public readonly IUnitConverter          Converter ;
    public readonly AttributeQuery<string>  Query     ;


    public AttributeDescriptor (string name, IUnitConverter converter, AttributeQuery<string> query)
    {
      Name      = name      ?? ""                         ;
      Converter = converter ?? UnitConverters.Scalar      ;
      Query     = query     ?? AttributeQuery.Return ("") ;
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

namespace AttributeQueryCs.Monad
{
  using Common;

  using static AttributeQuery;
  using static Common.UnitConverters;

  static class Capabilities
  {
    static AttributeDescriptor A (string name, IUnitConverter converter, AttributeQuery<string> query)
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
            , A ("duplexMode"               , Scalar                                                , SysDataParam ("capabilityDuplexMode"))
            , A ("txOperationBandLowEdge"   , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , freqClassUsage.Bind (Gpp3 ("bandLimits")).Bind (Select (0))) /* select "DL_f_min" */
            , A ("txOperationBandHighEdge"  , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , freqClassUsage.Bind (Gpp3 ("bandLimits")).Bind (Select (1))) /* select "DL_f_max" */
            , A ("rxOperationBandLowEdge"   , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , freqClassUsage.Bind (Gpp3 ("bandLimits")).Bind (Select (2))) /* select "UL_f_min" */
            , A ("rxOperationBandHighEdge"  , Frequency (kHz /* IWD */, kHz /* gpp3 */)             , freqClassUsage.Bind (Gpp3 ("bandLimits")).Bind (Select (3))) /* select "UL_f_max" */
            , A ("txMaximumBandwidth"       , Frequency (kHz /* IWD */, kHz /* match IWD range */)  , SysDataParam ("capabilityDlBw")) /* Observed values in sysDataParam 10000-75000*/
            , A ("rxMaximumBandwidth"       , Frequency (kHz /* IWD */, kHz /* match IWD range */)  , SysDataParam ("capabilityUlBw")) /* Observed values in sysDataParam 10000-75000*/
            )
          };
    }

    public static readonly CapabilityDescriptor[] AllCapabilities = CreateCapabilities ();
  }
}
