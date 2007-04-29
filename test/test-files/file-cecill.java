/*
 * Copyright ADAE, (2005)
 *
 * This software is a computer program whose purpose is to implement 
 * e-learning service.
 *
 * This software is governed by the CeCILL  license under French law and
 * abiding by the rules of distribution of free software.  You can  use, 
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info". 
 * 
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability. 
 * 
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or 
 * data to be ensured and,  more generally, to use and operate it in the 
 * same conditions as regards security. 
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
*/

package fr.gouv.adae.cybeo.content.ejb;
import javax.ejb.*;
import java.util.Date;
import java.sql.*;
import java.util.*;
import fr.gouv.adae.cybeo.common.util.*;
import fr.gouv.adae.cybeo.common.ejb.*;

/**
 * <p>Title       : ResourcesEJB</p>
 * <p>Description : Entity EJB for Resources</p>
 * @version $Revision$
 *
 * @ejb.bean type="CMP"
 *           name="Assets"
 *           local-jndi-name="fr.gouv.adae.cybeo.content.ejb.AssetsHome"
 *           display-name="Assets Bean"
 *           transaction-type="Container"
 *           schema="Assets"
 *           view-type="local"
 *
 * @ejb:home generate="local"
 * @ejb:interface generate="local"
 * @ejb.transaction type="Required"
 *
 * @ejb.pk class="fr.gouv.adae.cybeo.content.ejb.AssetsPk"
 *
 * @ejb.env-entry name="DataSourceRef" type="java.lang.String" value="${ejbdoclet.project.datasource}"
 * @ejb.resource-ref res-name="jdbc/${ejbdoclet.project.datasource}" res-type="javax.sql.DataSource" res-auth="Container"
 *
 * @jboss.resource-ref res-ref-name="jdbc/${ejbdoclet.project.datasource}" resource-name="${ejbdoclet.project.datasource}"
 * @jboss.resource-manager  res-man-class="javax.sql.DataSource"
 *                          res-man-jndi-name="java:/${ejbdoclet.project.datasource}"
 *                          res-man-name="${ejbdoclet.project.datasource}"
 *
 * @ejb.finder signature="Collection findAll()" unchecked="true" transaction-type="NotSupported"
 *
 * @ejb.persistence table-name="assets"
 * @jboss.persistence create-table="false" remove-table="false"
 *
 * @ejb:env-entry    name="tableName" type="java.lang.String" value="assets"
 * @ejb:env-entry    name="primaryKey" type="java.lang.String" value="name,skin"
 */


public abstract class AssetsEJB implements javax.ejb.EntityBean
{

  private String name;
  private long skin;
  private long resourceid;


  /**
   *  @ejb:interface-method
   *	@ejb:persistent-field
   *  @ejb:pk-field
   *  @ejb.persistence
   *    column-name="name"
   *  @jboss:column-name name
   *  @weblogic:dbms-column name
   *  @return name
  */

  public abstract String getName();
  /**
   *  @ejb:interface-method
   */

  public abstract void setName(String name);
  /**
   *  @ejb:interface-method
   *  @ejb:pk-field
   *	@ejb:persistent-field
   *  @ejb.persistence
   *    column-name="skin"
   *  @jboss:column-name skin
   *  @weblogic:dbms-column skin
   *  @return skin
  */
  public abstract long getSkin();
  /**
   *  @ejb:interface-method
   */

  public abstract void setSkin(long skin);
  /**
   *  @ejb:interface-method
   *	@ejb:persistent-field
   *  @ejb.persistence
   *    column-name="resourceid"
   *  @jboss:column-name resourceid
   *  @weblogic:dbms-column resourceid
   *  @return resourceid
  */
  public abstract long getResourceid();

  /**
   *  @ejb:interface-method
   */

  public abstract void setResourceid(long resourceid);

    /**
     * @ejb:create-method
     */
  public AssetsPk ejbCreate(String sIconName,long lSkinId,long lResourcesId) throws CreateException{
    setName(sIconName);
    setSkin(lSkinId);
    setResourceid(lResourcesId);
    return new  AssetsPk(sIconName,lSkinId);
  }
  /**
   * @return Class
   */
  protected Class getPrimaryKeyClass()
  {
    return fr.gouv.adae.cybeo.content.ejb.AssetsPk.class ;
  }
  public void ejbPostCreate() throws CreateException {}

  /**
   * @throws RemoveException
   * @ejb:interface-method
   */
  public void ejbRemove() throws RemoveException{}

}
