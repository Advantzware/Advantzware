#region "using"
using System;
using System.Configuration;
using System.Data;
using System.Collections;
using System.Web;
using System.Web.UI.WebControls;

using System.IO;
using System.Xml;

using System.Net.Mail;

#endregion
public class func 
{

public static DateTime str2date(String sDate, string sFormat)
{
  if (sDate == string.Empty) return DateTime.MinValue;
    if (sFormat == "CultureInfo.CurrentCulture.DateTimeFormat.ShortDatePattern")
        sFormat = System.Globalization.CultureInfo.CurrentCulture.DateTimeFormat.ShortDatePattern;
  sDate = sDate.Trim(); 
    if (sFormat.IndexOfAny("ms:".ToCharArray()) >= 0 && sDate.Length < 19)
    {
        if (sDate.Split(" ".ToCharArray()).Length > 1 && sDate.Length == 16)
        {
            sDate += ":00";
        }
        else sDate += " 00:00:00";
    } 
    return System.Xml.XmlConvert.ToDateTime(sDate, sFormat);
}



public static String date2str(DateTime dtDate, string sFormat)
{
  if (dtDate == DateTime.MinValue) return string.Empty;
    if (sFormat == "CultureInfo.CurrentCulture.DateTimeFormat.ShortDatePattern")
       sFormat = System.Globalization.CultureInfo.CurrentCulture.DateTimeFormat.ShortDatePattern;
    return System.Xml.XmlConvert.ToString(dtDate, sFormat);
}

public static void GetMenu(DropDownList ddlQuickJump, string sCaption) 
{
    ddlQuickJump.Items.Clear();
    ddlQuickJump.Items.Add(new ListItem("Back to menu", "menu.aspx"));

if ( func.CheckUserPermissions("[dbo].[usergroup]", "SA") )    ddlQuickJump.Items.Add(new ListItem("User Group Maint", "usergroup_list.aspx"));

if ( func.CheckUserPermissions("[dbo].[main_menu]", "SA") )    ddlQuickJump.Items.Add(new ListItem("Menu Maintenance", "main_menu_list.aspx"));

if ( func.CheckUserPermissions("[dbo].[user_master]", "SA") )    ddlQuickJump.Items.Add(new ListItem("User Maintenance", "user_master_list.aspx"));

    if (ddlQuickJump.Items.FindByText(sCaption) == null ) 
    {
        ddlQuickJump.Items.Add(new ListItem("", ""));
        ddlQuickJump.Items.FindByText("").Selected = true;
    } 
    else ddlQuickJump.Items.FindByText(sCaption).Selected = true;
}
public static SqlDataSource GetSqlDataSource(string sSQLSelect)
{
    ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];
    SqlDataSource sds = new SqlDataSource(cts.ProviderName, cts.ConnectionString, sSQLSelect);
    sds.DataSourceMode = SqlDataSourceMode.DataReader;
    return sds;
}

public static string BuildParameterName(string sFieldName)
{
    string sReturn = "";

    for (int i = 0; i < sFieldName.Length; ++i)
    {
        if (sFieldName[i] == '[' || sFieldName[i] == ']') continue;
        if ((sFieldName[i] >= 'A' && sFieldName[i] <= 'Z') ||
             (sFieldName[i] >= 'a' && sFieldName[i] <= 'z') ||
             (sFieldName[i] >= '0' && sFieldName[i] <= '9') ||
              sFieldName[i] == '_')
            sReturn += sFieldName[i];
        else
            sReturn += "_";
    }

    return sReturn;
}

public static string SqlBuilder(string sSQL, string sAddString)
{
    if (sAddString == string.Empty) return sSQL.Trim();

    int iPos = sSQL.ToLower().IndexOf(" where ");
    if (iPos < 0)
    {
        sAddString = " where " + sAddString + " ";
        int groupByPos = sSQL.ToLower().IndexOf(" group by ");
        if (groupByPos > 0)
        {
            sSQL = sSQL.Insert(groupByPos, sAddString);
        }
        else
        {
            int orderByPos = sSQL.ToLower().IndexOf(" order by ");
            if (orderByPos < 0)
                sSQL += sAddString;
            else
                sSQL = sSQL.Insert(orderByPos, sAddString);
        }
    }
    else
    {
        iPos += 7;
        if (sSQL.Length > iPos) sAddString += " And ";
        sSQL = sSQL.Insert(iPos, sAddString);
    }

    return sSQL.Trim();
}

public static string WhereBuilder(string sWhere, string sAdd)
{
    return WhereBuilder(sWhere, sAdd, "And");
}
public static string WhereBuilder(string sWhere, string sAdd, string sLogicalOperator)
{
    if (sAdd == string.Empty) return sWhere.Trim();
    if (sWhere == string.Empty) return sAdd;
    string sReturn = "";    
    if (sWhere.Trim().EndsWith(sLogicalOperator, StringComparison.OrdinalIgnoreCase))
        sReturn = sWhere.Trim() + " " + sAdd;
    else
        sReturn = sWhere.Trim() + " " + sLogicalOperator + " " + sAdd;
    return sReturn;        
}

public static void AddGlyph(GridView grid, GridViewRow item)
{
    Label glyph = new Label();
    glyph.Text = "&nbsp" + (grid.SortDirection == SortDirection.Ascending? "<IMG src=\"Images/down.gif\" border=0/>": "<IMG src=\"Images/up.gif\" border=0/>");

    for(int i=0;i<grid.Columns.Count;i++)
    {
        string colExpr = grid.Columns[i].SortExpression;
        if (colExpr != String.Empty && colExpr == grid.SortExpression) item.Cells[i].Controls.Add(glyph);
    if (item.Cells[i].Controls.Count > 0 && item.Cells[i].Controls[0] is LinkButton)
      ((LinkButton)(item.Cells[i].Controls[0])).CssClass = grid.HeaderStyle.CssClass;
        else item.Cells[i].CssClass = grid.HeaderStyle.CssClass;
    }
}

  //Date as ddl
public static void DateToDropDownList( DateTime dt, ref System.Web.UI.WebControls.DropDownList ddlDay, ref System.Web.UI.WebControls.DropDownList ddlMonth, ref System.Web.UI.WebControls.DropDownList ddlYear)
{
  for (int i = (dt.Year == 1?DateTime.Now.Year:dt.Year) - 50 ; i <= (dt.Year == 1?DateTime.Now.Year:dt.Year) + 50; i++)
    ddlYear.Items.Add(new System.Web.UI.WebControls.ListItem(i.ToString(), i.ToString()));

  if ( dt == DateTime.MinValue ) return;

  ddlDay.SelectedValue = dt.Day.ToString();
  ddlMonth.SelectedValue = dt.Month.ToString();
  ddlYear.SelectedValue = System.Convert.ToString(dt.Year == 1?DateTime.Now.Year:dt.Year);
}

public static object DropDownListToDate(ref System.Web.UI.WebControls.DropDownList ddlDay, ref System.Web.UI.WebControls.DropDownList ddlMonth, ref System.Web.UI.WebControls.DropDownList ddlYear) 
{
  string sDayRequiredDate, sMonthdtRequiredDate, sYeardtRequiredDate;

  sDayRequiredDate = ddlDay.SelectedValue;
  sMonthdtRequiredDate = ddlMonth.SelectedValue;
  sYeardtRequiredDate = ddlYear.SelectedValue;

  if ( sDayRequiredDate == "" && sMonthdtRequiredDate == "" && sYeardtRequiredDate == "" ) return null;
  else if ( sDayRequiredDate == "" || sMonthdtRequiredDate == "" || sYeardtRequiredDate == "" ) 
      throw new System.Exception("Invalid Datetime format");

  if ( sDayRequiredDate.Length < 2 ) sDayRequiredDate = "0" + sDayRequiredDate;
  if ( sMonthdtRequiredDate.Length < 2 ) sMonthdtRequiredDate = "0" + sMonthdtRequiredDate;

  return System.Xml.XmlConvert.ToDateTime(sDayRequiredDate + "/" + sMonthdtRequiredDate + "/" + sYeardtRequiredDate, "dd/MM/yyyy");
}

  //lookup column as ddl
public  static void LoadDataToLookUp(ref DropDownList ddlLookUp,  string  sSQLSelect,  string  sCurrentValue) 
{
    ddlLookUp.Items.Clear();
    ddlLookUp.DataSource = GetSqlDataSource(sSQLSelect);
    ddlLookUp.DataBind();
    ddlLookUp.Items.Insert(0, "");
    ddlLookUp.SelectedValue = null;    
    if (ddlLookUp.Items.FindByValue(sCurrentValue) != null) ddlLookUp.SelectedValue = sCurrentValue;
    else ddlLookUp.SelectedIndex = 0;
}

//lookup column as rbl
public  static void LoadDataToLookUp(ref System.Web.UI.WebControls.RadioButtonList ddlLookUp,  string  sSQLSelect,  string  sCurrentValue) 
{
    ddlLookUp.Items.Clear();
    ddlLookUp.DataSource = GetSqlDataSource(sSQLSelect);
    ddlLookUp.DataBind();
    ddlLookUp.Items.Insert(0, "");
    ddlLookUp.SelectedValue = null;    
    if (ddlLookUp.Items.FindByValue(sCurrentValue) != null) ddlLookUp.SelectedValue = sCurrentValue;
    else ddlLookUp.SelectedIndex = 0;
}

public  static void LoadDataToLookUp(ref System.Web.UI.WebControls.ListBox ddlLookUp,  string  sSQLSelect,  string  sCurrentValue) 
{
    ddlLookUp.Items.Clear();
    ddlLookUp.DataSource = GetSqlDataSource(sSQLSelect);
    ddlLookUp.DataBind();
    ddlLookUp.Items.Insert(0, "");
    ddlLookUp.SelectedValue = null;    
    foreach (string s in sCurrentValue.Split(','))
    {
        if ( ddlLookUp.Items.FindByValue(s) != null ) ddlLookUp.Items.FindByValue(s).Selected = true;
    }
    if (String.IsNullOrEmpty(ddlLookUp.SelectedValue)) ddlLookUp.SelectedIndex = 0;    
}

public static string  GetLookupValue( string  sDisplayField,  string  sLinkField,  string  sTable,  string  sValue,  TypeCode TType)
{
    return GetLookupValue(sDisplayField, sLinkField, sTable, sValue, TType, false);
}
public static string  GetLookupValue( string  sDisplayField,  string  sLinkField,  string  sTable,  string  sValue,  TypeCode TType, bool isMultiline)
{
    string sGetLookupValue = "";
    ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];
    
    string sqlSelect = "SELECT " + sDisplayField + " FROM " + sTable + " WHERE ";
    SqlDataSource sds  = new SqlDataSource();
    try
    {
        sds.ProviderName = cts.ProviderName;
        sds.ConnectionString = cts.ConnectionString;

        if (isMultiline)
        {
            int i = 0;
            foreach (string s in sValue.Split(','))
            {
                string paramName = "param" + i.ToString();
                sqlSelect += sLinkField + "=@" + paramName + " Or ";
                sds.SelectParameters.Add(paramName, TType, s);
                i += 1;
            }
           if (sqlSelect.Length > 2) sqlSelect = sqlSelect.Remove(sqlSelect.Length - 3);
        }
        else
        {
            sqlSelect += sLinkField + "= @LinkField";
            sds.SelectParameters.Add("LinkField", TType, sValue);
        }
        
        sds.SelectCommand = sqlSelect;
        sds.DataSourceMode = SqlDataSourceMode.DataReader;

        System.Data.Common.DbDataReader dbDr = (System.Data.Common.DbDataReader)sds.Select(System.Web.UI.DataSourceSelectArguments.Empty);
        if (dbDr.HasRows) 
        {
            if (isMultiline)
            {
                while (dbDr.Read()) {sGetLookupValue += dbDr[0].ToString() + ","; }               
                sGetLookupValue = sGetLookupValue.Trim(",".ToCharArray());
            }
            else if (dbDr.Read()) sGetLookupValue = dbDr[0].ToString();
        }    
    dbDr.Dispose();
    }    
    finally
    {
        sds.Dispose();
    }
    return sGetLookupValue;
}

//Upload binary file
public static object GetBinary(ref FileUpload _InputFile) 
{
  if (_InputFile.PostedFile != null )
  {
    int iImageSize = _InputFile.PostedFile.ContentLength;
        if ( iImageSize == 0 ) throw new System.Exception(" File '" + _InputFile.PostedFile.FileName + "' not found<br>");
        
    System.IO.Stream ImageStream = _InputFile.PostedFile.InputStream;
        byte[] ImageContent = new byte[iImageSize];
        ImageStream.Read(ImageContent, 0, iImageSize);
        return ImageContent;
  }
  else return DBNull.Value;
}

public static void DeleteFile( string  sFileName) 
{
  System.IO.FileInfo fiDescription = new System.IO.FileInfo(sFileName);
    if ( fiDescription.Exists ) fiDescription.Delete();
}    
 
public static string  GetNavigateUrl( string  sValue,  string  sLinkType) 
{
  return (sValue.StartsWith(sLinkType)?sValue:sLinkType + sValue);
}

public static string ProcessLargeText( string  sValue,  int iNumberOfChars, string tableName, string param) 
{
    string sProcessLargeText = "";
    if ( sValue.TrimStart().StartsWith("<a href")  || sValue == "" || sValue == "&nbsp;" ) return sValue;
    if ( iNumberOfChars > 0 ) 
    {
        if ( sValue.Length > iNumberOfChars && !sValue.TrimStart().StartsWith("<a href") ) 
        {
                sProcessLargeText = sValue.Substring(0, iNumberOfChars).Replace("'","\'")
                + " <a href=\"#\" onClick=\"javascript: pwin = window.open('',null,'height=300,width=400,status=yes,resizable=yes,toolbar=no,menubar=no,location=no,left=150,top=200,scrollbars=yes'); "
                + "pwin.location='" + tableName + "_fulltext.aspx?" + HttpUtility.HtmlEncode(param) + "';"
                + "return false;\">" + "More" + "&nbsp;...</a>";
        }
    }
    return sProcessLargeText; 
}

//
//
public static bool CheckSecurity( string  sTable,  string  sAction,  string  sOwnerID) 
{
  if (System.Web.HttpContext.Current.Session["User"] == null) return false;
  string  sUserID = ((UserClass)System.Web.HttpContext.Current.Session["User"]).UserID;
    return CheckUserPermissions(sTable, sAction) && ((sOwnerID == sUserID) || IsAdminUser() 
       || GetsAdvSecurityMethod(sTable) == "0" || (GetsAdvSecurityMethod(sTable) == "2" && sAction.ToLower().IndexOfAny("s".ToCharArray()) > -1) );
}

    public static bool CheckUserPermissions(string sTable, string sAction)
    {

        if (System.Web.HttpContext.Current.Session["User"] == null) return false;
        if (IsAdminUser()) return true;

        XmlDocument myXmlDocument = GetConfigXML("");

        if (myXmlDocument.DocumentElement.SelectSingleNode("/Security/Tables/Table") == null) return true;

        string sDefautPremissons = "";
        if (GetTableProp(sTable, "DEFAULT") != null) sDefautPremissons = GetTableProp(sTable, "DEFAULT").ToLower();
        sAction = sAction.ToLower();
        if (myXmlDocument.DocumentElement.SelectSingleNode("/Security/Tables/Table/UserGroups/User") == null) return (sDefautPremissons.IndexOfAny(sAction.ToCharArray()) > -1);

        string sUserName = ((UserClass)System.Web.HttpContext.Current.Session["User"]).GroupID;
        string sPath = "/Security/Tables/Table[@Name='" + sTable + "']/UserGroups/User[@Name='" + sUserName + "']";
        XmlNode node = myXmlDocument.DocumentElement.SelectSingleNode(sPath);

        if (node != null)
        {
            if (node.Attributes["Permission"] == null) return (sDefautPremissons.IndexOfAny(sAction.ToCharArray()) > -1);
            else return (node.Attributes["Permission"].Value.ToLower().IndexOfAny(sAction.ToCharArray()) > -1);
        }
        else return (sDefautPremissons.IndexOfAny(sAction.ToCharArray()) > -1);
    }
    

public static string GetCurrentUserID()
{
  if (System.Web.HttpContext.Current.Session["User"] == null) return "";
    return ((UserClass)(System.Web.HttpContext.Current.Session["User"])).UserID;
}

public static bool IsAdminUser()
{
    if (System.Web.HttpContext.Current.Session["User"] == null) return false;

    string  sUserAdmin = GetOptions("AdminID");
    string  sUserName = ((UserClass)System.Web.HttpContext.Current.Session["User"]).UserName;

    return sUserAdmin == sUserName && sUserAdmin != "";
}

public static string  GetsAdvSecurityMethod( string  sTableName) 
{
  return GetTableProp(sTableName, "AdvSecurityMethod");
}

public static string  GetOwnerIDField( string  sTableName) 
{
  return GetTableProp(sTableName, "OwnerIDField");
}

public static string  GetTableProp( string  sTableName,  string  sPropertyName)
{
    XmlDocument myXmlDocument = GetConfigXML("");
    string  sPath = "/Security/Tables/Table[@Name='" + sTableName + "']";
    XmlNode node = myXmlDocument.DocumentElement.SelectSingleNode(sPath);

  if (node == null) return "";
    else return node.Attributes[sPropertyName].Value;

}

public static string  GetOptions( string  sOptionsName) 
{
  string sGetOptions = "";
    try 
  {
    XmlDocument myXmlDocument = GetConfigXML("");
        XmlNode node = myXmlDocument.DocumentElement.SelectSingleNode("/Security/Options");
        sGetOptions = node.Attributes[sOptionsName].Value;
    }
  catch
  {
            //throw new System.Exception("Attribute '" + sOptionsName + "' not found in security options file :" + ex.Message)
    sGetOptions = "";
    }
    return sGetOptions;
}

private static XmlDocument GetConfigXML( string  sXmlFileName )
{
  if ( System.Web.HttpContext.Current.Session["XmlDocument"] == null ) 
  {
    try 
    {
      if ( sXmlFileName == "" ) sXmlFileName = ConfigurationManager.AppSettings["TablesFile"];
      XmlDocument myXmlDocument = new XmlDocument();
            myXmlDocument.Load(System.Web.HttpContext.Current.Server.MapPath(sXmlFileName));
            return myXmlDocument;
        } 
    catch (Exception ex)
    {
          throw new System.Exception("Security file '" + sXmlFileName + "' not correct : " + ex.Message);
    }
  }
  else return (XmlDocument)System.Web.HttpContext.Current.Session["XmlDocument"];      
}
//
public static void SendMail( string  mailTo,  string  subj,  string  body) 
{
    string mailSender = ConfigurationManager.AppSettings["MailSender"];
    string mailSmtpServer = ConfigurationManager.AppSettings["MailSmtpServer"];
    string mailSmtpPort = ConfigurationManager.AppSettings["MailSmtpPort"];
    string mailSMTPUser = ConfigurationManager.AppSettings["MailSMTPUser"];
    string mailSMTPPassword = ConfigurationManager.AppSettings["MailSMTPPassword"];

    if (string.IsNullOrEmpty(mailSender) || string.IsNullOrEmpty(mailTo)) return;
    MailMessage mail = new MailMessage(mailSender, mailTo, subj, body);

    //send the message
    SmtpClient smtp = new SmtpClient(mailSmtpServer);
    smtp.Port = Convert.ToInt32(mailSmtpPort);

    if (mailSMTPUser != null && mailSMTPUser != "")
    {
        //to authenticate we set the username and password properites on the SmtpClient
        smtp.Credentials = new System.Net.NetworkCredential(mailSMTPUser, mailSMTPPassword);
    }
    smtp.Send(mail);
}

public static bool IsDate(object dt)
{
  try
    {
        System.DateTime.Parse(dt.ToString());
        return true;
      }
      catch
      {
        return false;
      }
}
public static bool IsNumeric(object oValue)
{
  try
    {
        double.Parse(oValue.ToString());
        return true;
      }
      catch
      {
        return false;
      }
}
}//class func
public class UserClass {

    private Guid _id;
    //
    private string  _UserID = "";
  private string  _GroupID = "";
    //
    private string  _userName;
    private string  _password;

public Guid ID
{
    get {return _id;}
    set {_id = value;}
}

//
public string  UserID
{
  get {return _UserID;}
    set {_UserID = value;}
}
public string  GroupID
{
  get {return _GroupID;}
    set {_GroupID = value;}
}
//
public string  UserName
{
  get {return _userName;}
    set {_userName = value;}
}

public string Password
{
  get {return _password;}
  set {_password = value;}
}

public UserClass Login( string  txtUser,  string  txtPassword)
{
    
    UserClass  UserLogin = new UserClass();

    string  sLoginFrom = ConfigurationManager.AppSettings["LoginFrom"];
    if ( sLoginFrom == "DB" ) 
    { // User Name and Password from DB
        string  sTable = ConfigurationManager.AppSettings["UserListTable"];
        string  sLogin = ConfigurationManager.AppSettings["FieldUserLogin"];
        string  sLoginType = ConfigurationManager.AppSettings["FieldUserLoginType"];
        string  sPwd = ConfigurationManager.AppSettings["FieldUserPwd"];
        string  sPwdType = ConfigurationManager.AppSettings["FieldUserPwdType"];
    
    if ( sTable == "" || sLogin == "" || sPwd == "" )  return null;   

        string  sUserID = func.GetOptions("FieldUserOwnerID");
    string  sGroupID = func.GetOptions("FieldUserGroupID");

    string sSQL = "select [" + sLogin + "],[" + sPwd +"]";

    if (ConfigurationManager.AppSettings["TablesFile"] != string.Empty && sUserID != string.Empty) sSQL += ", [" + sUserID +"]";
    if (ConfigurationManager.AppSettings["TablesFile"] != string.Empty && sGroupID != string.Empty) sSQL += ", [" + sGroupID +"]";

    sTable = sTable.Replace(".", "].[");
    sSQL += " from [" + sTable + "] where [" + sLogin + "] = @Login and [" + sPwd + "] = @Password ";

        ConnectionStringSettings cts = ConfigurationManager.ConnectionStrings["Project1ConnectionString"];    
        SqlDataSource sds = new SqlDataSource(cts.ProviderName, cts.ConnectionString, sSQL);
        try
        {
            sds.DataSourceMode = SqlDataSourceMode.DataReader;
            sds.SelectParameters.Add("Login", (TypeCode)Enum.Parse(typeof(TypeCode), sLoginType), txtUser);
            sds.SelectParameters.Add("Password", (TypeCode)Enum.Parse(typeof(TypeCode), sPwdType) ,txtPassword);

            System.Data.Common.DbDataReader dbDr = (System.Data.Common.DbDataReader)sds.Select(System.Web.UI.DataSourceSelectArguments.Empty);
            
            if (dbDr.HasRows && dbDr.Read())
            {
                if ( dbDr[sLogin].ToString() == txtUser && dbDr[sPwd].ToString() == txtPassword ) 
                {
                    UserLogin.ID = Guid.NewGuid();

                    if (ConfigurationManager.AppSettings["TablesFile"] != "" && sUserID != string.Empty) UserLogin.UserID = Convert.ToString(dbDr[sUserID]);
                    if (ConfigurationManager.AppSettings["TablesFile"] != "" && sGroupID != string.Empty) UserLogin.GroupID = Convert.ToString(dbDr[sGroupID]);

                }
            }
            dbDr.Dispose();
        }
        finally
        {
            sds.Dispose();
        }
    }        
    else 
    { // hardcoded
        if ( txtUser == ConfigurationManager.AppSettings["UserLogin"] && txtPassword == ConfigurationManager.AppSettings["UserPassword"] ) 
            UserLogin.ID = Guid.NewGuid();
    }
            
  if ( UserLogin.ID == Guid.Empty ) return null;
  else 
  {
    UserLogin.UserName = txtUser;
    return UserLogin;
  }

}

public static void CheckLogin( System.Web.UI.Page Page) 
{
    if ( string.IsNullOrEmpty(ConfigurationManager.AppSettings["LoginMethod"]) || (ConfigurationManager.AppSettings["LoginMethod"]).ToUpper()  == "WITHOUTLOGIN" ) return;
//    Page.Response.CacheControl = "no-cache";
//    Page.Response.AddHeader("Pragma", "no-cache");
//    Page.Response.Expires = -1;
    if ( Page.Session["User"] == null ) Page.Response.Redirect(ConfigurationManager.AppSettings["LoginFile"] + "?url=" + Page.Request.RawUrl);
    if ( ((UserClass)Page.Session["User"]).ID == Guid.Empty) Page.Response.Redirect(ConfigurationManager.AppSettings["LoginFile"] + "?url=" + Page.Request.RawUrl);
}

}
