namespace AttributeQueryCs.Common
{
  using System;
  using System.Collections.Generic;

  sealed class ProductData
  {
    public string Get (string key)
    {
      return "TODO:";
    }
  }

  sealed class SysData
  {
    public string Get (string key)
    {
      return "TODO:";
    }
  }

  sealed class Gpp3Data
  {
    public string Get (string path, string key)
    {
      return "TODO:";
    }
  }

  sealed class QueryContext
  {
    public ProductData    ProductData   = new ProductData ();
    public SysData        SysData       = new SysData     ();
    public Gpp3Data       Gpp3Data      = new Gpp3Data    ();
    public List<string>   Errors        = new List<string>();

    [ThreadStatic]
    public static QueryContext Instance = new QueryContext();
  }

  interface IUnitConverter
  {
    string UnitName { get; }
    string Convert (string i);
  }

  static class UnitConverters
  {
    sealed class ScalarConverter : IUnitConverter
    {
      public string UnitName
      {
        get
        {
          return "Scalar";
        }
      }

      public string Convert(string i)
      {
        return i ?? "";
      }
    }

    public enum Frequencies
    {
      kHz ,
    }

    sealed class FrequencyConverter : IUnitConverter
    {
      public readonly Frequencies From  ;
      public readonly Frequencies To    ;

      public FrequencyConverter (Frequencies from, Frequencies to)
      {
        From  = from;
        To    = to  ;
      }

      public string UnitName
      {
        get
        {
          return "Frequency";
        }
      }

      public string Convert(string i)
      {
        return i ?? "";
      }
    }

    public enum Powers
    {
      dBm0_1  ,
    }

    sealed class PowerConverter : IUnitConverter
    {
      public readonly Powers From  ;
      public readonly Powers To    ;

      public PowerConverter (Powers from, Powers to)
      {
        From  = from;
        To    = to  ;
      }

      public string UnitName
      {
        get
        {
          return "Power";
        }
      }

      public string Convert(string i)
      {
        return i ?? "";
      }
    }

    public static readonly IUnitConverter Scalar = new ScalarConverter ();

    public static IUnitConverter Frequency (Frequencies from, Frequencies to)
    {
      return new FrequencyConverter (from ,to);
    }

    public static IUnitConverter Power (Powers from, Powers to)
    {
      return new PowerConverter (from ,to);
    }
  }

}
