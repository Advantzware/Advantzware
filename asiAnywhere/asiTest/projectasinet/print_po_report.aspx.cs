#region " using "
using System;
using System.Data;
using System.Web.UI.WebControls;
using System.Collections;
using System.Configuration;
using System.Threading;
using System.Globalization;
using System.Data.SqlClient;
#endregion

public partial class print_po_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "Brwslist_po.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            
            
            if (aUsers == "external")
            {
               
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        
        string sCulture = ConfigurationManager.AppSettings["LCID"];
        if (!String.IsNullOrEmpty(sCulture))
        {
            int nCulture = int.Parse(sCulture);
            System.Threading.Thread.CurrentThread.CurrentCulture = new System.Globalization.CultureInfo(nCulture, false);
        }

        if (!Page.IsPostBack)
        {
            OutPutFile.Visible = false;
            HyperLink1.Visible = false;
        
                        

            SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
            try
            {
                conn.Open();

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_po_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    bepoTextBox.Text = dr["field1"].ToString();
                    endpoTextBox.Text = dr["field2"].ToString();                   
                    bevendTextBox.Text       = dr["field3"].ToString();
                    endvendTextBox.Text       = dr["field4"].ToString();
                    
                    
                    

                    if (dr["chk_field1"].ToString() == "True")
                        CheckBox1.Checked = true;
                    else
                        CheckBox1.Checked = false;
                    if (dr["chk_field2"].ToString() == "True")
                        CheckBox2.Checked = true;
                    else
                        CheckBox2.Checked = false;
                    if (dr["chk_field3"].ToString() == "True")
                        CheckBox3.Checked = true;
                    else
                        CheckBox3.Checked = false;
                    if (dr["chk_field4"].ToString() == "True")
                        CheckBox4.Checked = true;
                    else
                        CheckBox4.Checked = false;

                    if (dr["chk_field5"].ToString() == "True")
                        CheckBox5.Checked = true;
                    else
                        CheckBox5.Checked = false;
                    if (dr["chk_field6"].ToString() == "True")
                        CheckBox6.Checked = true;
                    else
                        CheckBox6.Checked = false;
                    if (dr["chk_field7"].ToString() == "True")
                        CheckBox7.Checked = true;
                    else
                        CheckBox7.Checked = false;
                    if (dr["chk_field8"].ToString() == "True")
                        CheckBox8.Checked = true;
                    else
                        CheckBox8.Checked = false;
                    if (dr["chk_field9"].ToString() == "True")
                        CheckBox9.Checked = true;
                    else
                        CheckBox9.Checked = false;

                    if (dr["chk_field10"].ToString() == "True")
                        CheckBox10.Checked = true;
                    else
                        CheckBox10.Checked = false;
                    if (dr["chk_field11"].ToString() == "True")
                        CheckBox11.Checked = true;
                    else
                        CheckBox11.Checked = false;

                    if (dr["chk_field12"].ToString() == "True")
                        CheckBox12.Checked = true;
                    else
                        CheckBox12.Checked = false;

                    

                                 
                }

                conn.Close();
            }
            catch
            {
                conn.Close();
            }
            finally
            {
                conn.Close();
            }


            try
            {
                

                if (endvendTextBox.Text == "")
                    endvendTextBox.Text = "zzzzzzzz";                

                           


                //RadioButtonList1.SelectedIndex = 0; 
            }
            catch { }

            try
            {
                browspo chkfile = new browspo();
                DataSet db = new DataSet();
                db = chkfile.SelectPrintPoReport(UserLogin.UserName, "getvalue", 0, 0, "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");

                string getsysval = db.Tables[0].Rows[0][1].ToString();
                if (getsysval == "Xprint" || getsysval == "PeachTree" || getsysval == "Protagon" || getsysval == "PPI" || getsysval == "Packrite")
                {
                    CheckBox9.Enabled = true;
                    CheckBox10.Enabled = true;
                }
                else
                {
                    if (getsysval == "CentBox")
                    {
                        CheckBox9.Enabled = true;
                    }
                    else
                    {
                        CheckBox10.Enabled = true;
                    }

                    CheckBox9.Enabled = false;
                    CheckBox10.Enabled = false;
                    CheckBox11.Enabled = false;
                }

                if (getsysval == "Brick" || getsysval == "CSC" || getsysval == "Southpak" || getsysval == "Xprint" || getsysval == "PeachTree" || getsysval == "Asixprnt" || getsysval == "PPI" || getsysval == "CSC-GA" || getsysval == "Indiana" || getsysval == "Packrite" || getsysval == "Allwest" || getsysval == "ACPI" || getsysval == "CCC" || getsysval == "Protagon" || getsysval == "SouleMed" || getsysval == "Soule")
                {
                    CheckBox5.Enabled = true;
                }
                else
                {
                    CheckBox5.Enabled = false;
                }
                
                

                if (getsysval == "Indiana")
                {
                    CheckBox12.Enabled = true;
                }
                else
                {
                    CheckBox12.Enabled = false;
                }


                
            }
            catch { }
            
        }
    }

    protected void hlnkLogOut_Click(object sender, EventArgs e)
    {

        string sLoginURL = ConfigurationManager.AppSettings["LoginFile"];
        if (sLoginURL == "")
        {
            Response.Write("<script language=javascript>alert('" + "Login page isn’t set" + "!');</script>");
            return;
        }

        Page.Session.Clear();
        Response.Redirect(sLoginURL);
    }

    protected void Back_tomenu_Click(object sender, EventArgs e)
    {
        Response.Redirect("menu.aspx");
    }

    protected void submitbutton_click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (CheckBox1.Checked)
            HiddenField1.Value = "yes";
        else
            HiddenField1.Value = "no";
        if (CheckBox2.Checked)
            HiddenField2.Value = "yes";
        else
            HiddenField2.Value = "no";
        if (CheckBox3.Checked)
            HiddenField3.Value = "yes";
        else
            HiddenField3.Value = "no";
        if (CheckBox4.Checked)
            HiddenField4.Value = "yes";
        else
            HiddenField4.Value = "no";
        if (CheckBox5.Checked)
            HiddenField5.Value = "yes";
        else
            HiddenField5.Value = "no";
        if (CheckBox6.Checked)
            HiddenField6.Value = "yes";
        else
            HiddenField6.Value = "no";
        if (CheckBox7.Checked)
            HiddenField7.Value = "yes";
        else
            HiddenField7.Value = "no";
        if (CheckBox8.Checked)
            HiddenField8.Value = "yes";
        else
            HiddenField8.Value = "no";
        if (CheckBox9.Checked)
            HiddenField9.Value = "yes";
        else
            HiddenField9.Value = "no";
        if (CheckBox10.Checked)
            HiddenField10.Value = "yes";
        else
            HiddenField10.Value = "no";
        if (CheckBox11.Checked)
            HiddenField11.Value = "yes";
        else
            HiddenField11.Value = "no";
        if (CheckBox12.Checked)
            HiddenField12.Value = "yes";
        else
            HiddenField12.Value = "no";

        

        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmprtpo"].DefaultValue = "prtpo";
        ObjectDataSource1.SelectParameters["prmbegpo"].DefaultValue = bepoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendpo"].DefaultValue = endpoTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegvend"].DefaultValue = bevendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendvend"].DefaultValue = endvendTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmreprt"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["prmreprtcl"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmdelete"].DefaultValue = HiddenField3.Value.Trim();
        ObjectDataSource1.SelectParameters["prmprttrm"].DefaultValue = HiddenField4.Value.Trim();
        ObjectDataSource1.SelectParameters["prmspec"].DefaultValue = HiddenField5.Value.Trim();
        ObjectDataSource1.SelectParameters["prmcorr"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmgrpnts"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmsmmritm"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmitmdsr"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmscrtyp"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmmetric"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["prmprtprice"].DefaultValue = HiddenField12.Value; 
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_po_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6, chk_field7, chk_field8, chk_field9, chk_field10, chk_field11, chk_field12) values ('" + UserLogin.UserName + "','print_po_report.aspx' , '" + bepoTextBox.Text.Trim() + "', '" + endpoTextBox.Text.Trim() + "', '" + bevendTextBox.Text.Trim() + "', '" + endvendTextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + CheckBox4.Checked + "','" + CheckBox5.Checked + "','" + CheckBox6.Checked + "','" + CheckBox7.Checked + "','" + CheckBox8.Checked + "','" + CheckBox9.Checked + "','" + CheckBox10.Checked + "','" + CheckBox11.Checked + "','" + CheckBox12.Checked + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + bepoTextBox.Text.Trim() + "', field2 = '" + endpoTextBox.Text.Trim() + "', field3 = '" + bevendTextBox.Text.Trim() + "', field4 = '" + endvendTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', chk_field4 = '" + CheckBox4.Checked + "', chk_field5 = '" + CheckBox5.Checked + "', chk_field6 = '" + CheckBox6.Checked + "', chk_field7 = '" + CheckBox7.Checked + "', chk_field8 = '" + CheckBox8.Checked + "', chk_field9 = '" + CheckBox9.Checked + "', chk_field10 = '" + CheckBox10.Checked + "', chk_field11 = '" + CheckBox11.Checked + "', chk_field12 = '" + CheckBox12.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'print_po_report.aspx' ", conn);
                cmd_update.ExecuteNonQuery();
            }
            conn.Close();
        }
        catch (Exception ex)
        {
            Label1.Text = "Error :" + ex.Message + "<p>";
            conn.Close();
        }
        finally
        {
            conn.Close();
        }


        try
        {
            OutPutFile.Visible = true;
            HyperLink1.Visible = true;
            Label vpath = (Label)FormView1.FindControl("prtpoLabel");
            HyperLink1.Text = vpath.Text;
            HyperLink1.NavigateUrl = @"/pdfs/" + vpath.Text;


            if (vpath.Text == "")
            {
                OutPutFile.Text = "No CSV Exists";
                
            }
        }
        catch { }       
        
    }

   }
