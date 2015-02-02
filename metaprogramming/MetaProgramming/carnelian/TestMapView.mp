@@@ metaprogram
@@@ extension java
@@@ include Common.mp

package mrange

import junit.framework.TestCase;
import org.jmock.*;


@@> imports = $model.get(:imports, [])
@@> for imp in imports
import @@=imp=@@;
@@> end

public class TestGeneratedMaps
{
  public TestGeneratedMaps (String name)
  {
    super (name);
  }

  @Override
  protected void setUp ()
  {
  }

  @Override
  protected void tearDown()
  {
  }

@@> mapDefinitions = $model.get(:mapDefinitions, [])
@@> for mapDefinition in mapDefinitions
@@>   name                = mapDefinition.get :name
@@>   key_type            = mapDefinition.get :key_type
@@>   value_type          = mapDefinition.get :value_type
@@>   boxed_key_type      = mapDefinition.get(:boxed_key_type, key_type)
@@>   boxed_value_type    = mapDefinition.get(:boxed_value_type, value_type)
@@>   key_default_value   = key_type == boxed_key_type ? "null" : "0"
@@>   value_default_value = value_type == boxed_value_type ? "null" : "0"

  public void test@@=name=@@ ()
  {
    final @@=key_type=@@ firstSampleKey   = TestPmSampleData.getSampleDataFromPrototype (1, (@@=key_type=@@)@@=key_default_value=@@);
    final @@=key_type=@@ secondSampleKey  = TestPmSampleData.getSampleDataFromPrototype (2, (@@=key_type=@@)@@=key_default_value=@@);
    final @@=key_type=@@ thirdSampleKey   = TestPmSampleData.getSampleDataFromPrototype (3, (@@=key_type=@@)@@=key_default_value=@@);

    final @@=value_type=@@ defaultValue       = @@=value_default_value=@@;
    final @@=value_type=@@ firstSampleValue   = TestPmSampleData.getSampleDataFromPrototype (1, (@@=value_type=@@)@@=value_default_value=@@);
    final @@=value_type=@@ secondSampleValue  = TestPmSampleData.getSampleDataFromPrototype (2, (@@=value_type=@@)@@=value_default_value=@@);
    final @@=value_type=@@ thirdSampleValue   = TestPmSampleData.getSampleDataFromPrototype (3, (@@=value_type=@@)@@=value_default_value=@@);


    final @@=name=@@ map            = new @@=name=@@ ();
    assertEquals (0                 , map.size ());
    assertEquals (true              , map.isEmpty ());
    assertEquals (false             , map.containsKey (firstSampleKey));
    assertEquals (false             , map.containsKey (secondSampleKey));
    assertEquals (defaultValue      , map.get (firstSampleKey));
    assertEquals (thirdSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (defaultValue      , map.get (secondSampleKey));
    assertEquals (thirdSampleValue  , map.get (secondSampleKey, thirdSampleValue));

    final @@=value_type=@@[] emptyValues = map.getValues ();
    assertEquals (0, emptyValues.length);

    final boolean addFirstSampleResult    = map.add (firstSampleKey, firstSampleValue);
    assertEquals (true              , addFirstSampleResult);
    assertEquals (1                 , map.size ());
    assertEquals (false             , map.isEmpty ());
    assertEquals (true              , map.containsKey (firstSampleKey));
    assertEquals (false             , map.containsKey (secondSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (defaultValue      , map.get (secondSampleKey));
    assertEquals (thirdSampleValue  , map.get (secondSampleKey, thirdSampleValue));

    final boolean addSecondSampleResult   = map.add (secondSampleKey, secondSampleValue);
    assertEquals (true              , addSecondSampleResult);
    assertEquals (2                 , map.size ());
    assertEquals (false             , map.isEmpty ());
    assertEquals (true              , map.containsKey (firstSampleKey));
    assertEquals (true              , map.containsKey (secondSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (secondSampleValue , map.get (secondSampleKey));
    assertEquals (secondSampleValue , map.get (secondSampleKey, thirdSampleValue));

    final @@=value_type=@@[] values = map.getValues ();
    assertEquals (2                 , values.length);
    assertEquals (firstSampleValue  , values[0]);
    assertEquals (secondSampleValue , values[1]);

    // Negative test ccase
    final boolean addFirstAgentResult     = map.add (firstSampleKey, firstSampleValue);
    assertEquals (false             , addFirstAgentResult);
    assertEquals (2                 , map.size ());
    assertEquals (false             , map.isEmpty ());
    assertEquals (true              , map.containsKey (firstSampleKey));
    assertEquals (true              , map.containsKey (secondSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (secondSampleValue , map.get (secondSampleKey));
    assertEquals (secondSampleValue , map.get (secondSampleKey, thirdSampleValue));

    // Negative test ccase
    final boolean removeThirdSampleResult = map.remove (thirdSampleKey);
//    assertEquals (false             , removeThirdSampleResult);
    assertEquals (2                 , map.size ());
    assertEquals (false             , map.isEmpty ());
    assertEquals (true              , map.containsKey (firstSampleKey));
    assertEquals (true              , map.containsKey (secondSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey));
    assertEquals (firstSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (secondSampleValue , map.get (secondSampleKey));
    assertEquals (secondSampleValue , map.get (secondSampleKey, thirdSampleValue));

    final boolean removeFirstSampleResult = map.remove (firstSampleKey);
//    assertEquals (true              , removeFirstSampleResult);
    assertEquals (1                 , map.size ());
    assertEquals (false             , map.isEmpty ());
    assertEquals (false             , map.containsKey (firstSampleKey));
    assertEquals (true              , map.containsKey (secondSampleKey));
    assertEquals (defaultValue      , map.get (firstSampleKey));
    assertEquals (thirdSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (secondSampleValue , map.get (secondSampleKey));
    assertEquals (secondSampleValue , map.get (secondSampleKey, thirdSampleValue));

    map.clear ();
    assertEquals (0                 , map.size ());
    assertEquals (true              , map.isEmpty ());
    assertEquals (false             , map.containsKey (firstSampleKey));
    assertEquals (false             , map.containsKey (secondSampleKey));
    assertEquals (defaultValue      , map.get (firstSampleKey));
    assertEquals (thirdSampleValue  , map.get (firstSampleKey, thirdSampleValue));
    assertEquals (defaultValue      , map.get (secondSampleKey));
    assertEquals (thirdSampleValue  , map.get (secondSampleKey, thirdSampleValue));

    // TODO: Test exception cases of get/set
  }

@@> end
}
