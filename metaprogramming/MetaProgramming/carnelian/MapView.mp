@@@ metaprogram
@@@ extension java
@@@ include Common.mp

package mrange

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;

@@> imports = $model.get(:imports, [])
@@> for imp in imports
import @@=imp=@@;
@@> end

public class GeneratedMaps
{

@@> mapDefinitions = $model.get(:mapDefinitions, [])
@@> for mapDefinition in mapDefinitions
@@>   name                = mapDefinition.get :name
@@>   key_type            = mapDefinition.get :key_type
@@>   value_type          = mapDefinition.get :value_type
@@>   boxed_key_type      = mapDefinition.get(:boxed_key_type, key_type)
@@>   boxed_value_type    = mapDefinition.get(:boxed_value_type, value_type)
@@>   key_default_value   = key_type == boxed_key_type ? "null" : "0"
@@>   value_default_value = value_type == boxed_value_type ? "null" : "0"

  /**
   * @@=name=@@ - Typed HashMap of @@=key_type=@@*@@=value_type=@@
   */
  public static class @@=name=@@
  {
    private static final @@=value_type=@@ s_defaultValue = @@=value_default_value=@@;

    // Map of @@=key_type=@@(@@=boxed_key_type=@@)*@@=value_type=@@(@@=boxed_value_type=@@)
    private final HashMap m_map = new HashMap ();

    /**
     * Adds a key value pair to the HashMap
     *
     * @param key   Key value
     * @param value Value
     * @return true if Key doesn't exist in HashMap, false otherwise
     */
    public boolean add (@@=key_type=@@ key, @@=value_type=@@ value)
    {
@@>   if key_type == boxed_key_type
      @@=boxed_key_type=@@ k = key;
@@>   else
      @@=boxed_key_type=@@ k = new @@=boxed_key_type=@@ (key);
@@>   end

      if (m_map.containsKey (k))
      {
        return false;
      }

@@>   if value_type == boxed_value_type
      @@=boxed_value_type=@@ v = value;
@@>   else
      @@=boxed_value_type=@@ v = new @@=boxed_value_type=@@ (value);
@@>   end

      return m_map.put (k, v) == null;
    }

    /**
     * Tests if a key exists in HashMap
     *
     * @param key Key value
     * @return True if the key exists, false otherwise
     */
    public boolean containsKey (@@=key_type=@@ key)
    {
@@>   if key_type == boxed_key_type
      @@=boxed_key_type=@@ k = key;
@@>   else
      @@=boxed_key_type=@@ k = new @@=boxed_key_type=@@ (key);
@@>   end

      return m_map.containsKey (k);
    }

    /**
     * Gets a value from the HashMap, if the key is not found returns
     * defaultValue
     *
     * @param key           Key value
     * @param defaultValue  Value return if key is not found in HashMap
     * @return Value in HashMap if the key exists in HashMap,
     *         defaultValue otherwise
     */
    public @@=value_type=@@ get (@@=key_type=@@ key, @@=value_type=@@ defaultValue)
    {
@@>   if key_type == boxed_key_type
      @@=boxed_key_type=@@ k = key;
@@>   else
      @@=boxed_key_type=@@ k = new @@=boxed_key_type=@@ (key);
@@>   end

      @@=boxed_value_type=@@ v = (@@=boxed_value_type=@@) m_map.get (k);

      if (v == null)
      {
        return defaultValue;
      }

@@>   if value_type == boxed_value_type
      return v;
@@>   else
      return v.@@=value_type=@@Value ();
@@>   end
    }

    /**
     * Gets a value from the HashMap, if the key is not found returns
     * the default of the value type
     *
     * @param key           Key value
     * @return Value in HashMap if the key exists in HashMap,
     *         the default of the value type otherwise
     */
    public @@=value_type=@@ get (@@=key_type=@@ key)
    {
      return get (key, s_defaultValue);
    }

    /**
     * Removes a key from the HashMap
     *
     * @param key           Key value
     * @return True if key exists in HashMap, false otherwise
     */
    public boolean remove (@@=key_type=@@ key)
    {
@@>   if key_type == boxed_key_type
      @@=boxed_key_type=@@ k = key;
@@>   else
      @@=boxed_key_type=@@ k = new @@=boxed_key_type=@@ (key);
@@>   end

      return m_map.remove (k) != null;
    }

    public @@=value_type=@@[] getValues ()
    {
      @@=value_type=@@[] values = new @@=value_type=@@[m_map.size()];

      Collection  collection  = m_map.values()        ;
      Iterator    iterator    = collection.iterator() ;

      int idx = -1;
      while (iterator.hasNext())
      {
        @@=boxed_value_type=@@ value = (@@=boxed_value_type=@@) iterator.next();

@@>   if value_type == boxed_value_type
        values[++idx] = value;
@@>   else
        values[++idx] = value.@@=value_type=@@Value ();
@@>   end
      }

      return values;
    }

    /**
     * Tests if the HashMap is empty
     *
     * @return True if HashMap is empty, false otherwise
     */
    public boolean isEmpty ()
    {
      return m_map.isEmpty ();
    }

    /**
     * Returns the size of the HashMap
     *
     * @return The size of the HashMap
     */
    public int size ()
    {
      return m_map.size ();
    }

    /**
     * Clears the map
     */
    public void clear ()
    {
      m_map.clear ();
    }

  }

@@> end
}
