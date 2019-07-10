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

public partial class gl_trans_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "gl_trans_report.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            labelcompany.Text = PrmComp;
            Session["Customers_Company"] = labelcompany.Text;
                        
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gl_trans_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    bedtTextBox.Text = dr["field1"].ToString();
                    enddtTextBox.Text = dr["field2"].ToString();
                    beactTextBox.Text = dr["field3"].ToString();
                    endactTextBox.Text = dr["field4"].ToString();
                    
                    
                    

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
                    if (dr["chk_field13"].ToString() == "True")
                        CheckBox13.Checked = true;
                    else
                        CheckBox13.Checked = false;
                    if (dr["chk_field14"].ToString() == "True")
                        CheckBox14.Checked = true;
                    else
                        CheckBox14.Checked = false;
                    if (dr["chk_field15"].ToString() == "True")
                        CheckBox15.Checked = true;
                    else
                        CheckBox15.Checked = false;
                    if (dr["chk_field16"].ToString() == "True")
                        CheckBox16.Checked = true;
                    else
                        CheckBox16.Checked = false;
                    if (dr["chk_field17"].ToString() == "True")
                        CheckBox17.Checked = true;
                    else
                        CheckBox17.Checked = false;
                    if (dr["chk_field18"].ToString() == "True")
                        CheckBox18.Checked = true;
                    else
                        CheckBox18.Checked = false;
                    if (dr["chk_field19"].ToString() == "True")
                        CheckBox19.Checked = true;
                    else
                        CheckBox19.Checked = false;

                    

                                 
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


                if (endactTextBox.Text == "")
                    endactTextBox.Text = "99999-99";




                RadioButtonList3.SelectedIndex = 0; 
            }
            catch { }

            if (Session["User"] != null)
            {
                lblUser.Text = UserLogin.UserName;
            }
            
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
        if (CheckBox13.Checked)
            HiddenField13.Value = "yes";
        else
            HiddenField13.Value = "no";
        if (CheckBox14.Checked)
            HiddenField14.Value = "yes";
        else
            HiddenField14.Value = "no";
        if (CheckBox15.Checked)
            HiddenField15.Value = "yes";
        else
            HiddenField15.Value = "no";
        if (CheckBox15.Checked)
            HiddenField15.Value = "yes";
        else
            HiddenField15.Value = "no";
        if (CheckBox16.Checked)
            HiddenField16.Value = "yes";
        else
            HiddenField16.Value = "no";
        if (CheckBox17.Checked)
            HiddenField17.Value = "yes";
        else
            HiddenField17.Value = "no";
        if (CheckBox18.Checked)
            HiddenField18.Value = "yes";
        else
            HiddenField18.Value = "no";
        if (CheckBox19.Checked)
            HiddenField19.Value = "yes";
        else
            HiddenField19.Value = "no";        

        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "trnas";
        ObjectDataSource1.SelectParameters["prmbegdate"].DefaultValue = bedtTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddate"].DefaultValue = enddtTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegact"].DefaultValue = beactTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendact"].DefaultValue = endactTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmtb_cashr"].DefaultValue = HiddenField1.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_cashrvd"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_general"].DefaultValue = HiddenField3.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_apckr"].DefaultValue = HiddenField4.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_mcshrec"].DefaultValue = HiddenField5.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_apmem"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmtb_acpay"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmtb_appurch"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmtb_adjust"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmtb_fgpost"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmtb_autodist"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["prmtb_void_checks"].DefaultValue = HiddenField12.Value;
        ObjectDataSource1.SelectParameters["prmtb_arinv"].DefaultValue = HiddenField13.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_cdisb"].DefaultValue = HiddenField14.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_crmem"].DefaultValue = HiddenField15.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_oeinv"].DefaultValue = HiddenField16.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_apvoidck"].DefaultValue = HiddenField17.Value.Trim();
        ObjectDataSource1.SelectParameters["prmtb_jcost"].DefaultValue = HiddenField18.Value;
        ObjectDataSource1.SelectParameters["prmtb_rmpost"].DefaultValue = HiddenField19.Value;
        ObjectDataSource1.SelectParameters["prmOut"].DefaultValue = RadioButtonList3.SelectedValue;


        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'gl_trans_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6, chk_field7, chk_field8, chk_field9, chk_field10, chk_field11, chk_field12, chk_field13, chk_field14, chk_field15, chk_field16, chk_field17, chk_field18, chk_field19) values ('" + UserLogin.UserName + "','gl_trans_report.aspx' , '" + bedtTextBox.Text.Trim() + "', '" + enddtTextBox.Text.Trim() + "', '" + beactTextBox.Text.Trim() + "', '" + endactTextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + CheckBox4.Checked + "','" + CheckBox5.Checked + "','" + CheckBox6.Checked + "','" + CheckBox7.Checked + "','" + CheckBox8.Checked + "','" + CheckBox9.Checked + "','" + CheckBox10.Checked + "','" + CheckBox11.Checked + "','" + CheckBox12.Checked + "','" + CheckBox13.Checked + "','" + CheckBox14.Checked + "','" + CheckBox15.Checked + "','" + CheckBox16.Checked + "','" + CheckBox17.Checked + "','" + CheckBox18.Checked + "','" + CheckBox19.Checked + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + bedtTextBox.Text.Trim() + "', field2 = '" + enddtTextBox.Text.Trim() + "', field3 = '" + beactTextBox.Text.Trim() + "', field4 = '" + endactTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', chk_field4 = '" + CheckBox4.Checked + "', chk_field5 = '" + CheckBox5.Checked + "', chk_field6 = '" + CheckBox6.Checked + "', chk_field7 = '" + CheckBox7.Checked + "', chk_field8 = '" + CheckBox8.Checked + "', chk_field9 = '" + CheckBox9.Checked + "', chk_field10 = '" + CheckBox10.Checked + "', chk_field11 = '" + CheckBox11.Checked + "', chk_field12 = '" + CheckBox12.Checked + "', chk_field13 = '" + CheckBox13.Checked + "', chk_field14 = '" + CheckBox14.Checked + "', chk_field15 = '" + CheckBox15.Checked + "', chk_field16 = '" + CheckBox16.Checked + "', chk_field17 = '" + CheckBox17.Checked + "', chk_field18 = '" + CheckBox18.Checked + "', chk_field19 = '" + CheckBox19.Checked + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'gl_trans_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("trnasLabel");
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
