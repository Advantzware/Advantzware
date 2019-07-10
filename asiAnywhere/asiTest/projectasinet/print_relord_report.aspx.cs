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

public partial class print_relord_report : System.Web.UI.Page
{

    private bool bSort = true;

    protected void Page_Load(object sender, System.EventArgs e)
    {

        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
       
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "ReleaseOrd_list.aspx";
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

                string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_relord_report.aspx' ";
                SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
                DataSet ds = new DataSet();
                da.Fill(ds);

                foreach (DataRow dr in ds.Tables[0].Rows)
                {
                    becustTextBox.Text = dr["field1"].ToString();
                    endcustTextBox.Text = dr["field2"].ToString();
                    berellTextBox.Text = dr["field3"].ToString();
                    endrellTextBox.Text = dr["field4"].ToString();
                    begordTextBox.Text = dr["field5"].ToString();
                    endordTextBox.Text = dr["field6"].ToString();
                    bedateTextBox.Text = dr["field7"].ToString();
                    enddateTextBox.Text = dr["field8"].ToString();
                    bedelznTextBox.Text = dr["field9"].ToString();
                    enddelznTextBox.Text = dr["field10"].ToString();
                    fwhseTextBox.Text = dr["field11"].ToString();
                    twhseTextBox.Text = dr["field12"].ToString();
                    fbinTextBox.Text = dr["field13"].ToString();
                    tbinTextBox.Text = dr["field14"].ToString();
                    
                    
                    

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


                    if (dr["rd_field1"].ToString() == "I")
                        RadioButtonList1.SelectedIndex = 0;
                    if (dr["rd_field1"].ToString() == "R")
                        RadioButtonList1.SelectedIndex = 1;
                    if (dr["rd_field1"].ToString() == "S")
                        RadioButtonList1.SelectedIndex = 2;

                                 
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
                if (endcustTextBox.Text == "")
                    endcustTextBox.Text = "zzzzzzzz";
                if (endrellTextBox.Text == "")
                    endrellTextBox.Text = "99999999";
                if (endordTextBox.Text == "")
                    endordTextBox.Text = "99999999";
                if (enddelznTextBox.Text == "")
                    enddelznTextBox.Text = "zzzzz";
                if (twhseTextBox.Text == "")
                    twhseTextBox.Text = "zzzzz";
                if (tbinTextBox.Text == "")
                    tbinTextBox.Text = "zzzzzzzz";                

                           


                //RadioButtonList1.SelectedIndex = 0; 
            }
            catch { }

            try
            {
                release chkfile = new release();
                DataSet db = new DataSet();
                db = chkfile.PrintReleaseOrd(UserLogin.UserName, "getvalue", "", 0, 0, "", "", "", "", "", 0, 0, "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "");

                string getsysval = db.Tables[0].Rows[0][1].ToString();
                if (getsysval == "Xprint" )
                {
                    CheckBox8.Enabled = true;                    
                }
                else
                {
                    CheckBox8.Enabled = false;
                }
                if (getsysval == "Hughes")
                {
                    CheckBox13.Enabled = true;
                }
                else
                {
                    CheckBox13.Enabled = false;
                }
                if (getsysval == "Argrov")
                {
                    CheckBox5.Enabled = true;
                    CheckBox6.Enabled = true;
                }
                else
                {
                    CheckBox5.Enabled = false;
                    CheckBox6.Enabled = false;
                }
                if (getsysval == "NStock" || getsysval == "Metro")
                {
                    CheckBox4.Enabled = true;
                }
                else
                {
                    CheckBox4.Enabled = false;
                }
                if (getsysval == "Sonoco" || getsysval == "Rosmar")
                {
                    CheckBox10.Enabled = false;
                }
                else
                {
                    CheckBox10.Enabled = true;
                }              
                

                if (getsysval == "Indiana")
                {
                    CheckBox9.Enabled = true;
                }
                else
                {
                    CheckBox9.Enabled = false;
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
        if (CheckBox13.Checked)
            HiddenField13.Value = "yes";
        else
            HiddenField13.Value = "no";        

        if (RadioButtonList1.SelectedIndex == 0)
            HiddenField15.Value = "I";
        if (RadioButtonList1.SelectedIndex == 1)
            HiddenField15.Value = "R";
        if (RadioButtonList1.SelectedIndex == 2)
            HiddenField15.Value = "S";

        

        
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmrelprt"].DefaultValue = "relprt";
        ObjectDataSource1.SelectParameters["prmbegcust"].DefaultValue = becustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegrel"].DefaultValue = berellTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegord"].DefaultValue = begordTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdate"].DefaultValue = bedateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegdelz"].DefaultValue = bedelznTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbegloc"].DefaultValue = fwhseTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmbeglocbin"].DefaultValue = fbinTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendcust"].DefaultValue = endcustTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendrel"].DefaultValue = endrellTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendord"].DefaultValue = endordTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddate"].DefaultValue = enddateTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmenddelz"].DefaultValue = enddelznTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendloc"].DefaultValue = twhseTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmendlocbin"].DefaultValue = tbinTextBox.Text.Trim();
        ObjectDataSource1.SelectParameters["prmprinted"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmpsted"].DefaultValue = HiddenField2.Value.Trim();
        ObjectDataSource1.SelectParameters["prmrel_prfm"].DefaultValue = HiddenField3.Value.Trim();
        ObjectDataSource1.SelectParameters["prmbinloc"].DefaultValue = HiddenField4.Value.Trim();
        ObjectDataSource1.SelectParameters["prmsrtdelz"].DefaultValue = HiddenField5.Value.Trim();
        ObjectDataSource1.SelectParameters["prmprtdelz"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmasscomp"].DefaultValue = HiddenField7.Value;
        ObjectDataSource1.SelectParameters["prmcustprt"].DefaultValue = HiddenField8.Value;
        ObjectDataSource1.SelectParameters["prmprtpric"].DefaultValue = HiddenField9.Value;
        ObjectDataSource1.SelectParameters["prmsrtbinloc"].DefaultValue = HiddenField10.Value;
        ObjectDataSource1.SelectParameters["prmprtwht"].DefaultValue = HiddenField15.Value;
        ObjectDataSource1.SelectParameters["prmpstrel"].DefaultValue = HiddenField11.Value;
        ObjectDataSource1.SelectParameters["prmmulrel"].DefaultValue = HiddenField12.Value;
        ObjectDataSource1.SelectParameters["prmextagbin"].DefaultValue = HiddenField13.Value;        
        

        SqlConnection conn = new SqlConnection(ConfigurationManager.ConnectionStrings["Project1ConnectionString"].ToString());
        try
        {
            conn.Open();

            string cmd = "select * from report_maintance where user_name = '" + UserLogin.UserName + "' and prog_name = 'print_relord_report.aspx' ";
            SqlDataAdapter da = new SqlDataAdapter(cmd, conn);
            DataSet ds = new DataSet();
            da.Fill(ds);

            if (ds.Tables[0].Rows.Count == 0)
            {
                SqlCommand cmd_insert = new SqlCommand("insert into report_maintance (user_name, prog_name , field1, field2, field3, field4, field5, field6, field7, field8, field9, field10, field11, field12, field13, field14, chk_field1, chk_field2, chk_field3, chk_field4, chk_field5, chk_field6, chk_field7, chk_field8, chk_field9, chk_field10, chk_field11, chk_field12, chk_field13, rd_field1) values ('" + UserLogin.UserName + "','print_relord_report.aspx' , '" + becustTextBox.Text.Trim() + "', '" + endcustTextBox.Text.Trim() + "', '" + berellTextBox.Text.Trim() + "', '" + endrellTextBox.Text.Trim() + "','" + begordTextBox.Text.Trim() + "','" + endordTextBox.Text.Trim() + "','" + bedateTextBox.Text.Trim() + "','" + enddateTextBox.Text.Trim() + "','" + bedelznTextBox.Text.Trim() + "','" + enddelznTextBox.Text.Trim() + "','" + fwhseTextBox.Text.Trim() + "','" + twhseTextBox.Text.Trim() + "','" + fbinTextBox.Text.Trim() + "','" + tbinTextBox.Text.Trim() + "','" + CheckBox1.Checked + "','" + CheckBox2.Checked + "','" + CheckBox3.Checked + "','" + CheckBox4.Checked + "','" + CheckBox5.Checked + "','" + CheckBox6.Checked + "','" + CheckBox7.Checked + "','" + CheckBox8.Checked + "','" + CheckBox9.Checked + "','" + CheckBox10.Checked + "','" + CheckBox11.Checked + "','" + CheckBox12.Checked + "','" + CheckBox13.Checked + "','" + HiddenField15.Value + "')", conn);
                cmd_insert.ExecuteNonQuery();
            }
            else
            {
                SqlCommand cmd_update = new SqlCommand("update report_maintance set field1 = '" + becustTextBox.Text.Trim() + "', field2 = '" + endcustTextBox.Text.Trim() + "', field3 = '" + berellTextBox.Text.Trim() + "', field4 = '" + endrellTextBox.Text.Trim() + "', field5 = '" + begordTextBox.Text.Trim() + "', field6 = '" + endordTextBox.Text.Trim() + "', field7 = '" + bedateTextBox.Text.Trim() + "', field8 = '" + enddateTextBox.Text.Trim() + "', field9 = '" + bedelznTextBox.Text.Trim() + "', field10 = '" + enddelznTextBox.Text.Trim() + "', field11 = '" + fwhseTextBox.Text.Trim() + "', field12 = '" + twhseTextBox.Text.Trim() + "', field13 = '" + fbinTextBox.Text.Trim() + "', field14 = '" + tbinTextBox.Text.Trim() + "', chk_field1 = '" + CheckBox1.Checked + "', chk_field2 = '" + CheckBox2.Checked + "', chk_field3 = '" + CheckBox3.Checked + "', chk_field4 = '" + CheckBox4.Checked + "', chk_field5 = '" + CheckBox5.Checked + "', chk_field6 = '" + CheckBox6.Checked + "', chk_field7 = '" + CheckBox7.Checked + "', chk_field8 = '" + CheckBox8.Checked + "', chk_field9 = '" + CheckBox9.Checked + "', chk_field10 = '" + CheckBox10.Checked + "', chk_field11 = '" + CheckBox11.Checked + "', chk_field12 = '" + CheckBox12.Checked + "', chk_field13 = '" + CheckBox13.Checked + "', rd_field1 = '" + HiddenField15.Value + "' where user_name = '" + UserLogin.UserName + "' and prog_name =  'print_relord_report.aspx' ", conn);
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
            Label vpath = (Label)FormView1.FindControl("relprtLabel");
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
